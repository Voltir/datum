package datum.avrolib.data

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import datum.avrolib.data.errors.InvalidRecordOnRead
import datum.avrolib.schemas.AVRO_LOGICAL_TYPE
import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.properties.TextProp
import datum.patterns.schemas._
import higherkindness.droste.{Algebra, scheme}
import org.apache.avro.generic.{GenericData, GenericRecord}
import org.apache.avro.util.Utf8

import scala.collection.immutable.SortedMap
import scala.collection.JavaConverters._

object RecordReader {

  // TODO - REAL ERROR HANDLING!

  val algebra: Algebra[SchemaF, AnyRef => Data] = {

    def asData(tpe: Type, any: Any, props: PropertyMap): Data = {
      (tpe, any) match {
        case (_, null)                 => data.empty
        case (IntType, v: Int)         => data.integer(v)
        case (TextType, v: Utf8)       => data.text(v.toString)
        case (BooleanType, v: Boolean) => data.boolean(v)
        case (LongType, v: Long)       => data.long(v)
        case (FloatType, v: Float)     => data.float(v)
        case (DoubleType, v: Double)   => data.double(v)

        case (DateType, v: Int) =>
          val date = LocalDate.ofEpochDay(v)
          data.date(date)

        case (TimestampType, v: Long) if props(AVRO_LOGICAL_TYPE) == TextProp("timestamp-micros") =>
          data.timestamp(Instant.EPOCH.plus(v, ChronoUnit.MICROS))

        case (TimestampType, v: Long) if props(AVRO_LOGICAL_TYPE) == TextProp("timestamp-millis") =>
          data.timestamp(Instant.ofEpochMilli(v))

        case (DateTimeType, v: Utf8) =>
          val date = LocalDateTime.parse(v, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
          data.localTime(date)

        case (ZonedDateTimeType, v: Utf8) =>
          val date = ZonedDateTime.parse(v, DateTimeFormatter.ISO_ZONED_DATE_TIME)
          data.zonedTime(date)

        case (BytesType, v: java.nio.ByteBuffer) =>
          val copy = Array.ofDim[Byte](v.remaining())
          v.get(copy)
          data.bytes(copy)

        case (typ, other) => throw InvalidRecordOnRead(s"Expected a ${Type.asString(typ)} but got ${other.getClass}")
      }
    }

    Algebra {
      case ValueF(tpe, props) =>
        any =>
          asData(tpe, any, props)

      case ObjF(fields, _) => {
        case generic: GenericRecord =>
          val builder = SortedMap.newBuilder[String, Data]

          // Have to zip with the record's schema because the (avro) field name may not match the (datum) schema name
          // However, order is preserved, so we can simply zip them together
          fields.iterator.zip(generic.getSchema.getFields.asScala.iterator).foreach {
            case ((key, fn), avroField) =>
              builder += key -> fn(generic.get(avroField.name()))
          }
          data.obj(builder.result())

        case null => data.empty

        case other => throw InvalidRecordOnRead(s"Expected an obj but got ${other.getClass}")
      }

      case RowF(columns, _) => {
        case generic: GenericRecord =>
          // As in the case with Obj, zip with the avro fields to get the correct name
          val values = columns.zip(generic.getSchema.getFields.asScala).map {
            case (col, avroField) =>
              col.value.apply(generic.get(avroField.name()))
          }
          data.row(values)

        case null => data.empty

        case other => throw InvalidRecordOnRead(s"Expected a row but got ${other.getClass}")
      }

      case ArrayF(conforms, _) => {
        case array: GenericData.Array[AnyRef @unchecked] =>
          val values = array.iterator().asScala.map[Data](conforms)
          data.array(values.toVector)
        case null => data.empty

        case other => throw InvalidRecordOnRead(s"Expected an array but got ${other.getClass}")
      }

      case UnionF(alts, _) => {
        case generic: GenericRecord =>
          // Unions are "boxed" in a record to ensure they have properties
          generic.get("union") match {
            case avroUnion: GenericRecord =>
              val name = avroUnion.getSchema.getProp(datum.avrolib.schemas.ORIGINAL_NAME_KEY)
              data.union(name, alts(name)(avroUnion.get("schema")))
            case null =>
              data.empty
          }

        case null => data.empty

        case other => throw InvalidRecordOnRead(s"Expected a union but got ${other.getClass}")
      }
    }
  }

  def generateFor(schema: Schema): GenericRecord => Data = {
    val fn = scheme.cata(algebra).apply(schema)
    fn.asInstanceOf[GenericRecord => Data]
  }
}
