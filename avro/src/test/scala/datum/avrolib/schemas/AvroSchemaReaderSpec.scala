package datum.avrolib.schemas
import datum.gen.algebras.SchemaGen
import datum.patterns.properties._
import datum.patterns.schemas._
import datum.patterns.schemas
import org.apache.avro.{SchemaBuilder, Schema => AvroSchema}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

class AvroSchemaReaderSpec extends AnyWordSpec with Checkers with Matchers {

  "AvroSchemaReader" should {

    "be able to encode a simple schema" in {
      val simple = schemas.obj()(
        "foo" -> schemas.value(IntType),
        "bar" -> schemas.value(BooleanType)
      )

      val result = AvroSchemaWriter.write(simple)
      assert(result.getField("foo").schema().getType == AvroSchema.Type.INT)
      assert(result.getField("bar").schema().getType == AvroSchema.Type.BOOLEAN)
    }

    "read date types" in {
      val schema = schemas.obj()("date" -> schemas.value(DateType))
      val avro = AvroSchemaWriter.write(schema)
      val read = AvroSchemaReader.read(avro)

      read shouldBe schema
    }

    "work with unnamed columns" in {
      val csv = schemas.row("test" -> "works".prop)(
        schemas.Column(schemas.value(IntType)),
        schemas.Column(schemas.value(BooleanType)),
        schemas.Column(schemas.value(BooleanType), Some("Unnamed")),
        schemas.Column(schemas.value(BooleanType), Some("Unnamed1")),
        schemas.Column(schemas.value(TextType), Some("Unnamed1"))
      )
      val avro = AvroSchemaWriter.write(csv)
      val schema = AvroSchemaReader.read(avro)
      schema shouldBe csv
    }

    "work with repeated names" in {
      val csv = schemas.row("test" -> "works".prop)(
        schemas.col("foo", schemas.value(IntType)),
        schemas.col("foo", schemas.value(IntType)),
        schemas.col("foo", schemas.value(IntType))
      )
      val avro = AvroSchemaWriter.write(csv)
      val schema = AvroSchemaReader.read(avro)
      schema shouldBe csv
    }

    "work with unnamed unions" in {
      val schema = schemas.union()("" -> schemas.value(IntType), "Unnamed" -> schemas.value(IntType))
      val avro = AvroSchemaWriter.write(schema)
      val read = AvroSchemaReader.read(avro)
      assert(read == schema)
    }

    "read rows with unnamed columns that are unions" in {
      val schema = schemas.row()(
        Column(schemas.union()("" -> schemas.value(IntType))),
        Column(schemas.union()("Unnamed" -> schemas.value(IntType)))
      )
      val avro = AvroSchemaWriter.write(schema)
      val read = AvroSchemaReader.read(avro)
      assert(read == schema)
    }

    "read arrays" in {
      val schema = schemas.array()(schemas.value(IntType))
      val avro = AvroSchemaWriter.write(schema)
      val read = AvroSchemaReader.read(avro)
      assert(read == schema)
    }

    "read more complicated array" in {
      val schema =
        schemas.array()(
          schemas.array()(schemas.union("foo" -> true.prop)("" -> schemas.array()(schemas.value(IntType)))))
      val avro = AvroSchemaWriter.write(schema)
      val read = AvroSchemaReader.read(avro)
      assert(read == schema)
    }

    "read avro schemas not originally created from writing datum schemas" in {
      val testUnion = AvroSchema.createUnion(
        AvroSchema.create(AvroSchema.Type.INT),
        AvroSchema.create(AvroSchema.Type.STRING)
      )
      val avro = SchemaBuilder
        .record("foo")
        .fields()
        .name("a")
        .`type`(AvroSchema.create(AvroSchema.Type.INT))
        .noDefault()
        .name("b")
        .`type`(testUnion)
        .noDefault()
        .endRecord()

      val read = AvroSchemaReader.read(avro)
      pprint.pprintln(read)
    }

    "be able to encode/decode an arbitrary schema to avro" in {
      val initial = Gen.oneOf(SchemaGen.AnObj, SchemaGen.ARow).map(SchemaGen.Seed(_, 5))

      implicit val arb: Arbitrary[Schema] = Arbitrary {
        //val fn = SchemaGen.define(SchemaGen.optional(SchemaGen.default.coalgebra))
        val fn = SchemaGen.define(SchemaGen.simple.coalgebra)
        initial.flatMap(fn)
      }

      check {
        forAll { schema: Schema =>
          val avro = AvroSchemaWriter.write(schema)
          val recovered = AvroSchemaReader.read(avro)
          recovered == schema
        }
      }
    }
  }
}
