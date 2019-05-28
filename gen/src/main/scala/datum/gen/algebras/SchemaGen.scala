package datum.gen.algebras
import datum.modifiers.Optional
import datum.patterns.schemas._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import qq.droste.{CoalgebraM, scheme}

import scala.collection.immutable.SortedMap

class SchemaGen(
  allowedValueTypes: Vector[Type] = SchemaGen.types.all,
  next: Gen[SchemaGen.Next] = SchemaGen.next.default,
  maxFields: Int = 5
) {
  import SchemaGen._

  private def nest(level: Int): Gen[Seed] = {
    for {
      n <- {
        if (level > 0) next
        else Gen.const(AValue)
      }
      l <- if (level > 1) Gen.chooseNum(0, level - 1) else Gen.const(0)
    } yield Seed(n, l)
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

  private def genTable(level: Int): Gen[SchemaF[Seed]] = {
    val col: Gen[Column[Seed]] = for {
      k <- Gen.option(Gen.resize(3, Gen.asciiPrintableStr))
      s <- nest(level)
    } yield Column(s, k)

    Gen.resize(maxFields, Gen.nonEmptyContainerOf[Vector, Column[Seed]](col).map { elements =>
      RowF[Seed](elements)
    })
  }

  private def genIndexed(level: Int): Gen[SchemaF[Seed]] = {
    Gen.resize(
      3,
      Gen.nonEmptyContainerOf[Vector, Seed](nest(level)).map { alts =>
        IndexedUnionF[Seed](alts)
      }
    )
  }

  private def genUnion(level: Int): Gen[SchemaF[Seed]] = {
    val alts = for {
      k <- Gen.resize(5, Gen.alphaLowerStr)
      s <- nest(level)
    } yield (k, s)

    Gen.resize(maxFields, Gen.nonEmptyListOf(alts)).map { choices =>
      NamedUnionF[Seed](SortedMap(choices: _*))
    }
  }

  private def genArray(level: Int): Gen[SchemaF[Seed]] = {
    nest(level).map { s =>
      ArrayF[Seed](s)
    }
  }

  val coalgebra: CoalgebraM[Gen, SchemaF, Seed] = CoalgebraM {
    case Seed(AValue, _)             => genValue
    case Seed(AnObj, level)          => genObj(level)
    case Seed(ATable, level)         => genTable(level)
    case Seed(AUnion, level)         => genUnion(level)
    case Seed(AnIndexedUnion, level) => genIndexed(level)
    case Seed(AnArray, level)        => genArray(level)
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
      1 -> Gen.const(ATable),
      1 -> Gen.const(AUnion),
      1 -> Gen.const(AnIndexedUnion),
      1 -> Gen.const(AnArray)
    )
  }

  sealed trait Next extends Serializable with Product
  final case object AValue extends Next
  final case object AnObj extends Next
  final case object ATable extends Next
  final case object AUnion extends Next
  final case object AnIndexedUnion extends Next
  final case object AnArray extends Next

  case class Seed(next: Next, level: Int)

  val default = new SchemaGen()

  val simple =
    new SchemaGen(
      allowedValueTypes = Vector(IntType, BooleanType, TextType),
      next = Gen.const(AValue)
    )

  def optional(alg: CoalgebraM[Gen, SchemaF, Seed]): CoalgebraM[Gen, SchemaF, Seed] = CoalgebraM { seed =>
    alg(seed).flatMap(schema => Gen.oneOf(schema, schema.withAttributes(Optional.enable)))
  }

  def define(coalg: CoalgebraM[Gen, SchemaF, Seed] = default.coalgebra): Seed => Gen[Schema] = {
    val fn = scheme.anaM(coalg)
    val result: Seed => Gen[Schema] = { seed =>
      fn(seed)
    }
    result
  }
}
