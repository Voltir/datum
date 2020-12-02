package datum.avrolib.data
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeUnit

import datum.avrolib.data.errors.InvalidRecordOnWrite
import datum.modifiers.Optional
import datum.patterns.data
import datum.patterns.data._
import datum.patterns.schemas._
import datum.patterns.properties._
import higherkindness.droste.data.{AttrF, Fix}
import higherkindness.droste.data.prelude._
import higherkindness.droste.syntax.all._
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import org.apache.avro.generic.GenericData.Record
import org.apache.avro.generic.GenericRecord
import org.apache.avro.{Schema => AvroSchema}
import org.apache.avro.generic.GenericRecordBuilder

import scala.collection.JavaConverters._

class RecordWriter(toAvro: Schema => AvroSchema) {

  type SchemaWithAvro[A] = AttrF[SchemaF, Option[AvroSchema], A]

  val annotate: Coalgebra[SchemaWithAvro, Schema] = Coalgebra { schema =>
    schema.project match {
      case obj @ ObjF(_, _) =>
        AttrF(Some(toAvro(schema)), obj)

      case obj @ RowF(_, _) =>
        AttrF(Some(toAvro(schema)), obj)

      case named @ UnionF(_, _) =>
        AttrF(Some(toAvro(schema)), named)

      case otherwise => AttrF(None, otherwise)
    }
  }

  val algebra: Algebra[SchemaWithAvro, Data => Any] = {

    def extract(tpe: Type, d: DataF[Data]): Any = {
      (tpe, d) match {
        case (IntType, data.IntValue(v))         => v
        case (TextType, data.TextValue(v))       => v
        case (LongType, data.LongValue(v))       => v
        case (BooleanType, data.BooleanValue(v)) => v
        case (DoubleType, data.DoubleValue(v))   => v
        case (FloatType, data.FloatValue(v))     => v
        case (DateType, data.DateValue(v))       => v.toEpochDay.toInt
        case (TimestampType, data.TimestampValue(v)) =>
          TimeUnit.SECONDS.toMicros(v.getEpochSecond()) + TimeUnit.NANOSECONDS.toMicros(v.getNano()) // expects micros
        case (DateTimeType, data.LocalDateTimeValue(v))      => v.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        case (ZonedDateTimeType, data.ZonedDateTimeValue(v)) => v.format(DateTimeFormatter.ISO_ZONED_DATE_TIME)
        case (BytesType, data.BytesValue(v))                 => java.nio.ByteBuffer.wrap(v)
        case (_, data.EmptyValue)                            => null
        case (typ, record) =>
          val msg = s"Expected a ${Type.asString(typ)} but got a ${data.typeOf(record.fix)}"
          throw InvalidRecordOnWrite(msg)
      }
    }

    Algebra[SchemaWithAvro, Data => Any] {

      case AttrF(None, ValueF(tpe, _)) =>
        d =>
          extract(tpe, Fix.un[DataF](d))

      case AttrF(Some(avro), ObjF(fields, _)) =>
        _.project match {
          case ObjValue(values) =>
            val generic = new GenericRecordBuilder(avro)

            // Have to zip with avro fields because fields may have been renamed
            avro.getFields.asScala.zip(fields).foreach {
              case (avroField, (key, fn)) =>
                val d = values.getOrElse(key, empty)
                generic.set(avroField, fn(d))
            }
            generic.build()

          case otherwise =>
            val msg = s"Expected an obj but got a ${data.typeOf(otherwise.fix)}"
            throw InvalidRecordOnWrite(msg)
        }

      case AttrF(Some(avro), RowF(columns, _)) =>
        _.project match {
          case RowValue(values) =>
            assert(!avro.isUnion, "Unexpected union type while handling row")
            val generic = new GenericRecordBuilder(avro)
            val records = columns.zip(values).map {
              case (col, v) =>
                col.value.apply(v)
            }
            avro.getFields.asScala.zip(records).foreach {
              case (avroField, record) =>
                generic.set(avroField, record)
            }
            generic.build()

          case otherwise =>
            val msg = s"Expected a row but got a ${data.typeOf(otherwise.fix)}"
            throw InvalidRecordOnWrite(msg)
        }

      case AttrF(None, ArrayF(conforms, _)) =>
        _.project match {
          case RowValue(values) =>
            values.map(conforms).asJava
          case otherwise =>
            val msg = s"Expected an array but got a ${data.typeOf(otherwise.fix)}"
            throw InvalidRecordOnWrite(msg)
        }

      case AttrF(Some(avro), UnionF(alts, _)) =>
        _.project match {
          case UnionValue(selection, value) =>
            // Union record must be written with the original name set as a prop
            val avroUnion = avro.getField("union").schema()
            val selected = avroUnion.getTypes.asScala.find { s =>
              selection == s.getProp(datum.avrolib.schemas.ORIGINAL_NAME_KEY)
            }

            val generic = new Record(selected.get)
            generic.put("schema", alts(selection)(value))

            val boxed = new Record(avro)
            boxed.put("union", generic)
            boxed

          case otherwise =>
            val msg = s"Expected a named-union but got a ${data.typeOf(otherwise.fix)}"
            throw InvalidRecordOnWrite(msg)
        }

      case AttrF(_, _) =>
        val msg = s"Invalid schema when writing record!"
        throw InvalidRecordOnWrite(msg)
    }
  }

  def optional(algebra: Algebra[SchemaWithAvro, Data => Any]): Algebra[SchemaWithAvro, Data => Any] = {

    def unwrap(schema: SchemaF[Data => Any], avro: AvroSchema): Data => Any = {
      assert(schema.properties.get(Optional.key).contains(true.prop), "Expected schema to be optional!")
      assert(avro.getTypes.size() == 2, "Unexpected UNION size for an optional row!")
      assert(avro.getTypes.get(1).getType == AvroSchema.Type.NULL, "NULL part of union not in expected position!")
      val unwrapped = avro.getTypes.get(0)

      {
        case data.empty => null
        case otherwise  => algebra(AttrF(Some(unwrapped), schema))(otherwise)
      }
    }

    Algebra[SchemaWithAvro, Data => Any] {

      // Avro uses unions to create optional/nullable values - unwrap them to correctly handle options
      case AttrF(Some(avro), obj @ ObjF(_, _)) if avro.getType == AvroSchema.Type.UNION => unwrap(obj, avro)

      case AttrF(Some(avro), row @ RowF(_, _)) if avro.getType == AvroSchema.Type.UNION => unwrap(row, avro)

      case AttrF(Some(avro), union @ UnionF(_, _)) if avro.getType == AvroSchema.Type.UNION => unwrap(union, avro)

      case other @ AttrF(_, schema) if schema.properties.get(Optional.key).contains(true.prop) => {
        case data.empty => null
        case otherwise  => algebra(other)(otherwise)
      }

      case otherwise => algebra(otherwise)
    }
  }

  def generateFor(schema: Schema): Data => GenericRecord = {
    // Functor for AttrF[...] comes from 'import higherkindness.droste.data.prelude._'
    val annotated = scheme.ana(annotate).apply(schema)
    val fn = scheme.cata(algebra).apply(annotated)
    fn.asInstanceOf[Data => GenericRecord]
  }

  def define(algebra: Algebra[SchemaWithAvro, Data => Any]): Schema => Data => GenericRecord = { schema =>
    val annotated = scheme.ana(annotate).apply(schema)
    val fn = scheme.cata(algebra).apply(annotated)
    fn.asInstanceOf[Data => GenericRecord]
  }
}
