package datum.gen
import datum.algebras.corresponds.Corresponds
import datum.gen.algebras.SchemaGen
import datum.gen.algebras.SchemaGen._
import datum.modifiers.Optional
import datum.patterns.{data, schemas}
import datum.patterns.properties._
import datum.patterns.schemas.{BooleanType, IntType, Schema, SchemaF}
import higherkindness.droste.data.Fix
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers

class SchemaGenSpec extends WordSpec with Checkers with Matchers {

  private val correspondsTo = Corresponds.define(Corresponds.optional(Corresponds.algebra))

  val ASeed: Gen[Seed] = Gen.oneOf(AnObj, AnArray, ARow, AUnion, AnIndexedUnion).map { Seed(_, 5) }

  "Schema Generation" should {
    "support optional values" in {
      val fn = SchemaGen.define(SchemaGen.optional(SchemaGen.default.coalgebra))
      implicit val gen: Arbitrary[Schema] = Arbitrary {
        Gen.oneOf(AnObj, AnArray, ARow).flatMap(n => fn(Seed(n, 5)))
      }
      check {
        forAll { schema: Schema =>
          val testFn = correspondsTo(schema)
          // todo: change back to schema.project.attributes if ambiguous implicit issue gets resolved
          val isOpt = Fix.un[SchemaF](schema).properties.get(Optional.key).contains(true.prop)
          testFn(data.empty) == isOpt
        }
      }
    }

    "support optional union types" in {
      val requiredUnion = schemas.indexed()(schemas.value(IntType), schemas.value(BooleanType))
      val optUnion1 =
        schemas.indexed(Optional.key -> true.prop)(schemas.value(IntType), schemas.value(BooleanType))
      val optUnion2 =
        schemas.indexed()(schemas.value(IntType, Optional.key -> true.prop), schemas.value(BooleanType))
      correspondsTo(requiredUnion)(data.empty) shouldBe false
      correspondsTo(optUnion1)(data.empty) shouldBe true
      correspondsTo(optUnion2)(data.indexed(0, data.empty)) shouldBe true
    }

    "unique colmns are unique" in {
      val unique = new SchemaGen(uniqueColumnNames = true, maxFields = 30)
      val fn = SchemaGen.define(unique.coalgebra)

      // This will generate a non-nested "flat" shape, akin to a simple csv
      implicit val gen: Arbitrary[Schema] = Arbitrary {
        fn(Seed(ARow, 0))
      }

      check {
        forAll { schema: Schema =>
          Fix.un[SchemaF](schema) match {
            case schemas.RowF(cols, _) =>
              val names = cols.map(_.header.getOrElse(""))
              names.length == names.distinct.length
            case _ => false
          }
        }
      }
    }
  }
}
