package datum.gen.algebras
import datum.modifiers.Optional
import datum.patterns.schemas._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import higherkindness.droste.{CoalgebraM, scheme}

import scala.collection.immutable.SortedMap
import scala.collection.mutable

class SchemaGen(
  allowedValueTypes: Vector[Type] = SchemaGen.types.all,
  next: Gen[SchemaGen.Next] = SchemaGen.next.default,
  maxFields: Int = 5,
  uniqueColumnNames: Boolean = false,
  generateProperties: Boolean = true,
) {
  import SchemaGen._

  private val genProperty = PropertyGen.define()

  private def nest(level: Int): Gen[Seed] = {
    for {
      n <- {
        if (level > 0) next
        else Gen.const(AValue)
      }
      l <- if (level > 1) Gen.chooseNum(0, level - 1) else Gen.const(0)
    } yield Seed(n, l)
  }

  private def addProperties(gen: Gen[SchemaF[Seed]]): Gen[SchemaF[Seed]] = {
    if (generateProperties) {
      for {
        schema <- gen
        props <- Gen.resize(3, Gen.nonEmptyListOf(genProperty))
      } yield schema.withProperties(props: _*)
    } else gen
  }

  private val genValue: Gen[SchemaF[Seed]] = Gen.oneOf(allowedValueTypes).map { x =>
    ValueF(x)
  }
  private def genObj(level: Int): Gen[SchemaF[Seed]] = {
    val genField = for {
      k <- Gen.resize(5, Gen.alphaLowerStr)
      s <- nest(level)
    } yield (k, s)

    Gen.resize(maxFields, Gen.nonEmptyListOf(genField).map { fields =>
      ObjF[Seed](SortedMap(fields: _*))
    })
  }

  private def genRow(level: Int): Gen[SchemaF[Seed]] = {
    val aName = Gen.resize(3, Gen.asciiPrintableStr)
    val aColumn: Gen[Column[Seed]] = Gen.zip(aName, nest(level)).map { case (s, k) => Column(k, Some(s)) }

    def uniqueColumns(gen: Gen[Column[Seed]]): Gen[Vector[Column[Seed]]] = {
      val names: mutable.Set[String] = mutable.Set.empty
      val seen: mutable.Buffer[Column[Seed]] = mutable.Buffer.empty

      val maxDiscarded = 100
      var discarded = 0

      Gen.sized { size =>
        if (size == seen.size) seen.toVector
        else {
          while (seen.size <= size && discarded < maxDiscarded) gen.sample match {
            case Some(col @ Column(_, Some(name))) if !names.contains(name) =>
              seen += col
              names += name
            case _ => discarded += 1
          }
          seen.toVector
        }
      }
    }

    if (uniqueColumnNames) {
      Gen.resize(maxFields, uniqueColumns(aColumn)).map { elements =>
        RowF[Seed](elements)
      }
    } else {
      Gen.resize(maxFields, Gen.nonEmptyContainerOf[Vector, Column[Seed]](aColumn).map { elements =>
        RowF[Seed](elements)
      })
    }
  }

  private def genUnion(level: Int): Gen[SchemaF[Seed]] = {
    val alts = for {
      k <- Gen.resize(5, Gen.alphaLowerStr)
      s <- nest(level)
    } yield (k, s)

    Gen.resize(maxFields, Gen.nonEmptyListOf(alts)).map { choices =>
      UnionF[Seed](SortedMap(choices: _*))
    }
  }

  private def genArray(level: Int): Gen[SchemaF[Seed]] = {
    nest(level).map { s =>
      ArrayF[Seed](s)
    }
  }

  val coalgebra: CoalgebraM[Gen, SchemaF, Seed] = CoalgebraM {
    case Seed(AValue, _)             => addProperties(genValue)
    case Seed(AnObj, level)          => addProperties(genObj(level))
    case Seed(ARow, level)           => addProperties(genRow(level))
    case Seed(AUnion, level)         => addProperties(genUnion(level))
    case Seed(AnArray, level)        => addProperties(genArray(level))
  }
}

object SchemaGen {

  object types {

    val all: Vector[Type] = Vector(
      IntType,
      LongType,
      TimestampType,
      DoubleType,
      FloatType,
      TextType,
      BytesType,
      BooleanType,
      DateType,
      DateTimeType,
      ZonedDateTimeType
    )
  }

  object next {

    val default: Gen[Next] = Gen.frequency(
      5 -> Gen.const(AValue),
      1 -> Gen.const(AnObj),
      1 -> Gen.const(ARow),
      1 -> Gen.const(AUnion),
      1 -> Gen.const(AnArray)
    )
  }

  sealed trait Next extends Serializable with Product
  final case object AValue extends Next
  final case object AnObj extends Next
  final case object ARow extends Next
  final case object AUnion extends Next
  final case object AnArray extends Next

  case class Seed(next: Next, level: Int)

  val default = new SchemaGen()

  val simple =
    new SchemaGen(
      allowedValueTypes = Vector(IntType, BooleanType, TextType),
      next = Gen.const(AValue),
      generateProperties = false
    )

  def optional(alg: CoalgebraM[Gen, SchemaF, Seed]): CoalgebraM[Gen, SchemaF, Seed] = CoalgebraM { seed =>
    alg(seed).flatMap(schema => Gen.oneOf(schema, schema.withProperties(Optional.enable)))
  }

  def define(coalg: CoalgebraM[Gen, SchemaF, Seed] = default.coalgebra): Seed => Gen[Schema] = {
    val fn = scheme.anaM(coalg)
    val result: Seed => Gen[Schema] = { seed =>
      fn(seed)
    }
    result
  }
}
