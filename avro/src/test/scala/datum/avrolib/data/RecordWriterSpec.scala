package datum.avrolib.data

import java.time._

import datum.avrolib.schemas.AvroSchemaWriter
import datum.avrolib.util.TestSchemas
import datum.modifiers.Optional
import datum.patterns.{data => d}
import datum.patterns.schemas
import datum.patterns.schemas._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class RecordWriterSpec extends AnyWordSpec with Checkers with Matchers {

  val recordWriter = new RecordWriter(AvroSchemaWriter.write)

  "Avrolib RecordWriter" should {
    "encode data matching a simple schema" in {
      val simple = schemas.obj()(
        "foo" -> schemas.value(IntType),
        "bar" -> schemas.value(BooleanType, Optional.enable)
      )
      val r1 = d.obj("foo" -> d.integer(10), "bar" -> d.boolean(true))
      val toGenericRecord = recordWriter.generateFor(simple)
      val generic = toGenericRecord(r1)

      generic.get("foo") shouldBe 10
      generic.get("bar") shouldBe true
    }

    "encode all value types" in {
      val toGenericRecord = recordWriter.generateFor(TestSchemas.types)

      val r1 = d.obj(
        "int" -> d.integer(1),
        "long" -> d.long(2),
        "float" -> d.float(3.0f),
        "double" -> d.double(4.0),
        "text" -> d.text("foo"),
        "bytes" -> d.bytes(Array(0.toByte, 255.toByte, 1.toByte)),
        "bool" -> d.boolean(true),
        "date" -> d.date(LocalDate.of(1970, 1, 1)),
        "timestamp" -> d.timestamp(Instant.ofEpochSecond(1000)),
        "date_time" -> d.localTime(LocalDateTime.of(1970, 1, 1, 10, 10, 10)),
        "zoned_date_time" -> d.zonedTime(
          ZonedDateTime.ofInstant(Instant.ofEpochSecond(1000), ZoneId.of("America/Los_Angeles")))
      )

      val generic = toGenericRecord(r1)

      generic.get("int") shouldBe 1
      generic.get("long") shouldBe 2
      generic.get("float") shouldBe 3.0f
      generic.get("double") shouldBe 4.0
      generic.get("text") shouldBe "foo"
      // generic.get("bytes") shouldBe Array(0.toByte, 255.toByte, 1.toByte)
      generic.get("bool") shouldBe true
      generic.get("date") shouldBe LocalDate.of(1970, 1, 1).toEpochDay
      //generic.get("timestamp") shouldBe Instant.ofEpochSecond(1000)
      //generic.get("date_time") shouldBe LocalDateTime.of(1970, 1, 1, 10, 10, 10)
      generic.get("zoned_date_time") shouldBe ZonedDateTime.ofInstant(
        Instant.ofEpochSecond(1000),
        ZoneId.of("America/Los_Angeles")
      ).toString
    }

    "handle optional fields" in {
      val simple = schemas.obj()(
        "foo" -> schemas.value(IntType),
        "bar" -> schemas.value(BooleanType, Optional.enable)
      )
      val r1 = d.obj("foo" -> d.integer(10), "bar" -> d.empty)
      val toGenericRecord = recordWriter.define(recordWriter.optional(recordWriter.algebra))(simple)

      val generic = toGenericRecord(r1)
      generic.get("bar") shouldBe null
    }
  }
}
