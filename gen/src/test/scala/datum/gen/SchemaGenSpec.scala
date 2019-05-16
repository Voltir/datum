package datum.gen

import datum.algebras.Corresponds
import datum.gen.algebras.SchemaGen
import datum.gen.algebras.SchemaGen._
import datum.modifiers.Optional
import datum.patterns.{data, schemas}
import datum.patterns.attributes._
import datum.patterns.schemas.{BooleanType, IntType, Schema}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers
import qq.droste.syntax.all._

class SchemaGenSpec extends WordSpec with Checkers with Matchers {

  val correspondsTo = Corresponds.define(Corresponds.optional(Corresponds.algebra))

  val ASeed: Gen[Seed] = Gen.oneOf(AnObj, AnArray, ATable, AUnion, AnIndexedUnion).map { Seed(_, 5) }

  "Schema Generation" should {
    "support optional values" in {
      implicit val gen: Arbitrary[Schema] = Arbitrary {
        val fn = SchemaGen.define(SchemaGen.optional(SchemaGen.default.coalgebra))
        Gen.oneOf(AnObj, AnArray, ATable).flatMap(n => fn(Seed(n, 5)))
      }
      check {
        forAll { schema: Schema =>
          val testFn = correspondsTo(schema)
          val isOpt = schema.project.attributes.contains(Optional.key)
          testFn(data.empty) == isOpt
        }
      }
    }

    "support optional union types" in {
      val requiredUnion = schemas.indexed()(schemas.value(IntType), schemas.value(BooleanType))
      val optUnion1 =
        schemas.indexed(Optional.key -> property(true))(schemas.value(IntType), schemas.value(BooleanType))
      val optUnion2 =
        schemas.indexed()(schemas.value(IntType, Optional.key -> property(true)), schemas.value(BooleanType))
      correspondsTo(requiredUnion)(data.empty) shouldBe false
      correspondsTo(optUnion1)(data.empty) shouldBe true
      correspondsTo(optUnion2)(data.indexed(0, data.empty)) shouldBe true
    }
  }
}
