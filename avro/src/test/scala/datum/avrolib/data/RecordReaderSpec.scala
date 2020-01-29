package datum.avrolib.data
import datum.avrolib.util.{Roundtrip, TestSchemas}
import datum.gen.algebras.{DataGen, SchemaGen}
import datum.modifiers.Optional
import datum.patterns.data.{Data, DataF, EmptyValue, ObjValue}
import datum.patterns.{data, schemas}
import datum.patterns.schemas._
import higherkindness.droste.{Algebra, scheme}
import higherkindness.droste.data.Fix
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

class RecordReaderSpec extends AnyWordSpec with Checkers with Matchers {

  // Normalize missing values to data.empty to make comparing optional values easier
  private val normalize = scheme.cata {
    Algebra[SchemaF, Data => Data] {
      case ObjF(fields, _) =>
        Fix.un[DataF](_) match {
          case ObjValue(values) =>
            val missing = fields.keySet.diff(values.keySet)
            val normalized = values.map { case (k, v) => k -> fields(k)(v) } ++ missing.map { k =>
              k -> fields(k)(data.empty)
            }
            data.obj(normalized)
          case EmptyValue =>
            data.obj(fields.mapValues(_.apply(data.empty)))
        }
      case _ => identity
    }
  }

  def assertRoundtrip(schema: Schema, optional: Boolean = false): Assertion = {
    implicit val arb: Arbitrary[List[Data]] = Arbitrary {
      val generator =
        if (optional) DataGen.define(DataGen.optional(DataGen.algebra))
        else DataGen.define()
      for {
        data <- Gen.nonEmptyListOf(generator(schema))
      } yield data
    }
    val norm = normalize(schema)

    check {
      forAll { data: List[Data] =>
        val results = Roundtrip(schema, data)
        data.zip(results).forall { x =>
          norm(x._1) == norm(x._2)
        }
      }
    }
  }

  "Avrolib RecordReader" should {

    "encode data for an union" in {
      val schema = schemas.obj()(
        "other" -> schemas.union()("a" -> schemas.value(BooleanType), "" -> schemas.value(TextType))
      )
      assertRoundtrip(schema)
    }

    "encode repeated unions" in {
      val schema = schemas.obj()(
        "union1" -> schemas.union()("a" -> schemas.value(BooleanType), "" -> schemas.value(TextType)),
        "union2" -> schemas.union()("a" -> schemas.value(IntType), "" -> schemas.value(IntType))
      )
      assertRoundtrip(schema)
    }

    "encode arrays" in {
      val schema = schemas.row()(
        schemas.col("array1", schemas.array()(schemas.value(IntType))),
        schemas.col("array2", schemas.array()(schemas.value(BooleanType)))
      )
      assertRoundtrip(schema)
    }

    "roundtrip all datatypes" in {
      assertRoundtrip(TestSchemas.types)
    }

    "roundtrip optional" in {
      val schema = schemas.obj()(
        "foo" -> schemas.value(IntType, Optional.enable),
        "bar" -> schemas.value(BooleanType, Optional.enable)
      )
      assertRoundtrip(schema, optional = true)
    }

    "roundtrip optional obj" in {
      val schema = schemas.obj()(
        "foo" -> schemas.obj(Optional.enable)("maybe" -> schemas.value(IntType, Optional.enable))
      )
      assertRoundtrip(schema, optional = true)
    }

    "roundtrip optional row" in {
      val schema = schemas.obj()(
        "foo" -> schemas.row(Optional.enable)(schemas.col("maybe", schemas.value(IntType, Optional.enable)))
      )
      assertRoundtrip(schema, optional = true)
    }

    "roundtrip optional array" in {
      val schema = schemas.obj()(
        "foo" -> schemas.array(Optional.enable)(schemas.value(IntType, Optional.enable))
      )
      assertRoundtrip(schema, optional = true)
    }

    "roundtrip optional union" in {
      val schema = schemas.obj()(
        "foo" -> schemas.union(Optional.enable)(
          "a" -> schemas.value(IntType, Optional.enable),
          "b" -> schemas.value(IntType, Optional.enable)
        )
      )
      assertRoundtrip(schema, optional = true)
    }

    "roundtrip arbitrary schemas and data" in {
      val generator = SchemaGen.default.coalgebra
      val initial = Gen.oneOf(SchemaGen.AnObj, SchemaGen.ARow).map(SchemaGen.Seed(_, 5))

      implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary {
        for {
          seed <- initial
          schema <- SchemaGen.define(generator)(seed)
          data <- Gen.nonEmptyListOf(DataGen.define()(schema))
        } yield (schema, data)
      }

      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, records) = generated
          Roundtrip(schema, records) == records
        }
      }
    }

    "handle optional values with renaming of fields occurring" in {
      val schema = schemas.row()(
        schemas.col("_1", schemas.value(TextType, Optional.enable)),
        schemas.col("2", schemas.value(BooleanType, Optional.enable))
      )
      assertRoundtrip(schema, optional = true)
    }

    "roundtrip arbitrary optional schemas and data" in {
      val generator = SchemaGen.optional(SchemaGen.simple.coalgebra)
      val initial = Gen.oneOf(SchemaGen.AnObj, SchemaGen.ARow).map(SchemaGen.Seed(_, 5))

      implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary {
        for {
          seed <- initial
          schema <- SchemaGen.define(generator)(seed)
          data <- Gen.nonEmptyListOf(DataGen.define(DataGen.optional(DataGen.algebra))(schema))
        } yield (schema, data)
      }

      check {
        forAll { generated: (Schema, List[Data]) =>
          // Due to the way avro encodes records, the top level record can't be optional
          !generated._1.properties.contains(Optional.key) ==> {
            val (schema, records) = generated
            val norm = normalize(schema)
            val results = Roundtrip(schema, records)
            records.zip(results).forall { x =>
              norm(x._1) == norm(x._2)
            }
          }
        }
      }
    }
  }
}
