package datum.gen
import datum.algebras.corresponds.Corresponds
import datum.gen.algebras.DataGen
import datum.modifiers.Optional
import datum.patterns.data.{BytesValue, Data, DataF, ZonedTimeValue}
import datum.patterns.{data, schemas}
import datum.patterns.schemas._
import higherkindness.droste.data.Fix
import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers

class DataGenSpec extends WordSpec with Checkers with Matchers {

  val test: Schema = schemas.obj()(
    "foo" -> schemas.value(IntType, Optional.enable),
    "bar" -> schemas.value(TextType)
  )

  val other: Schema = schemas.obj()(
    "no" -> schemas.value(IntType)
  )

  val generator = DataGen.define()

  val testGen = generator(test)

  val correspondsTo = Corresponds.define(Corresponds.optional(Corresponds.algebra))

  val correspondsTest: Data => Boolean = correspondsTo(test)

  val correspondsOther: Data => Boolean = correspondsTo(other)

  "Generated Data" should {
    "correspond to a particular schema" in {
      implicit val arb: Arbitrary[Data] = Arbitrary(testGen)
      check {
        forAll { data: Data =>
          correspondsTest(data) && !correspondsOther(data)
        }
      }
    }

    "generate optional values" in {
      val schema = schemas.array()(schemas.value(IntType, Optional.enable))
      val dataOf = DataGen.define(DataGen.optional(DataGen.algebra))
      val testFn = correspondsTo(schema)

      val sample = data.row(data.integer(1), data.empty, data.integer(2))
      testFn(sample) shouldBe true

      implicit val arb: Arbitrary[Data] = Arbitrary(dataOf(schema))

      check {
        forAll { data: Data =>
          testFn(data)
        }
      }
    }

    "generate zoned date time" in {
      val schema = schemas.value(ZonedDateTimeType)
      val dates = DataGen.define(DataGen.algebra)

      implicit val arb: Arbitrary[Data] = Arbitrary(dates(schema))
      check {
        forAll { data: Data =>
          Fix.un[DataF](data) match {
            case ZonedTimeValue(_) => true
            case _                 => false
          }
        }
      }
    }

    "generate bytes values" in {
      val schema = schemas.value(BytesType)
      val dates = DataGen.define(DataGen.algebra)

      implicit val arb: Arbitrary[Data] = Arbitrary(dates(schema))
      check {
        forAll { data: Data =>
          Fix.un[DataF](data) match {
            case BytesValue(_) => true
            case _             => false
          }
        }
      }
    }
  }
}
