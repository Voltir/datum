package datum.avrolib.schemas
import datum.gen.algebras.SchemaGen
import datum.patterns.schemas
import datum.patterns.schemas._
import org.apache.avro.{Schema => AvroSchema}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

class AvroSchemaWriterSpec extends AnyWordSpec with Checkers with Matchers {

  "AvroSchemaWriter" should {
    "be able to encode a simple schema" in {
      val simple = schemas.obj()(
        "foo" -> schemas.value(IntType),
        "bar" -> schemas.value(BooleanType)
      )

      val avro = AvroSchemaWriter.write(simple)

      assert(avro.getField("foo").schema().getType == AvroSchema.Type.INT)
      assert(avro.getField("bar").schema().getType == AvroSchema.Type.BOOLEAN)
    }

    "work with date types" in {
      val schema = schemas.obj()("date" -> schemas.value(DateType))
      val avro = AvroSchemaWriter.write(schema)

      avro.getField("date").schema().getProp(RECORD_TYPE_KEY) shouldBe "date"
    }

    "encode unions" in {
      val schema = schemas.union()("foo" -> schemas.value(IntType), "bar" -> schemas.value(IntType))
      val avro = AvroSchemaWriter.write(schema)
      avro.getType shouldBe AvroSchema.Type.RECORD
      avro.getField("union") shouldNot be(null)
      avro.getField("union").schema().getType shouldBe AvroSchema.Type.UNION
    }

    "encode arbitrary schemas" in {
      val initial = Gen.oneOf(SchemaGen.AnObj, SchemaGen.ARow).map(SchemaGen.Seed(_, 5))

      implicit val arb: Arbitrary[Schema] = Arbitrary {
        val fn = SchemaGen.define(SchemaGen.optional(SchemaGen.default.coalgebra))
        initial.flatMap(fn)
      }
      noException shouldBe thrownBy {
        check {
          forAll { schema: Schema =>
            AvroSchemaWriter.write(schema)
            true
          }
        }
      }
    }
  }
}
