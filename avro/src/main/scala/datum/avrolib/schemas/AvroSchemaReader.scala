package datum.avrolib.schemas
import datum.patterns.schemas._
import datum.avrolib.properties._
import datum.avrolib.schemas.errors.UnparseableAvroSchema
import datum.patterns.properties.{Property, TextProp}
import higherkindness.droste.{Coalgebra, scheme}
import org.apache.avro.LogicalTypes.{TimestampMicros, TimestampMillis, Date => DateLogicalType}
import org.apache.avro.util.internal.JacksonUtils
import org.apache.avro.{Schema => AvroSchema}

import scala.collection.JavaConverters._
import scala.collection.immutable.SortedMap

object AvroSchemaReader {

  val coalgebra: Coalgebra[SchemaF, AvroSchema] = Coalgebra[SchemaF, AvroSchema] { avro =>
    avro.getType match {
      case AvroSchema.Type.INT =>
        avro.getLogicalType match {
          case _: DateLogicalType => ValueF(DateType, extractProps(avro))
          case _                  => ValueF(IntType, extractProps(avro))
        }

      case AvroSchema.Type.FLOAT   => ValueF(FloatType, extractProps(avro))
      case AvroSchema.Type.DOUBLE  => ValueF(DoubleType, extractProps(avro))
      case AvroSchema.Type.BOOLEAN => ValueF(BooleanType, extractProps(avro))
      case AvroSchema.Type.STRING =>
        avro.getProp(RECORD_TYPE_KEY) match {
          case "date"            => ValueF(DateType, extractProps(avro))
          case "date-time"       => ValueF(DateTimeType, extractProps(avro))
          case "zoned-date-time" => ValueF(ZonedDateTimeType, extractProps(avro))
          case _                 => ValueF(TextType, extractProps(avro))
        }
      case AvroSchema.Type.LONG =>
        avro.getLogicalType match {
          case _: TimestampMillis =>
            ValueF(TimestampType, extractProps(avro) + (AVRO_LOGICAL_TYPE -> TextProp("timestamp-millis")))
          case _: TimestampMicros =>
            ValueF(TimestampType, extractProps(avro) + (AVRO_LOGICAL_TYPE -> TextProp("timestamp-micros")))
          case _ => ValueF(LongType, extractProps(avro))
        }
      case AvroSchema.Type.BYTES => ValueF(BytesType, extractProps(avro))

      case AvroSchema.Type.RECORD =>
        val fields = avro.getFields.asScala
        avro.getProp(RECORD_TYPE_KEY) match {

          // If RECORD_TYPE_KEY is not set, default to using an Obj for the Schema
          case "obj" | null =>
            val objFields = SortedMap.newBuilder[String, AvroSchema]
            fields.foreach { field =>
              objFields += (originalName(field) -> field.schema())
            }
            ObjF(objFields.result(), extractProps(avro))

          case "row" =>
            val columns = fields.view.map { field =>
              // Avro Union types can't carry properties despite being a (avro) "Schema" :/
              val noHeader = if (field.schema().isUnion) {
                true
              } else {
                field.schema().getObjectProp(NO_HEADER).asInstanceOf[Boolean]
              }

              val header =
                if (noHeader) None
                else Some(originalName(field))

              Column(field.schema(), header)
            }.toVector

            RowF(columns, extractProps(avro))

          case "union" =>
            val avroUnion = avro.getField("union")
            assert(avroUnion.schema().isUnion, "Expected the union field to be of type Union!")
            val alts = SortedMap.newBuilder[String, AvroSchema]
            avroUnion.schema().getTypes.asScala.foreach { alt =>
              val original = alt.getProp(ORIGINAL_NAME_KEY) match {
                case null => alt.getName
                case orig => orig
              }
              alts += (original -> alt.getField("schema").schema())
            }

            UnionF(alts.result(), extractProps(avro))

          case err => throw UnparseableAvroSchema(s"Unexpected RECORD_TYPE_KEY property when parsing avro: $err!")
        }

      case AvroSchema.Type.ARRAY =>
        ArrayF(avro.getElementType, extractProps(avro))

      // If we happen to get an unwrapped union (ie this schema was NOT created by writing a datum.schema)
      // Then we will treat it as a UnionF with no properties, and assume nothing about the schema
      case AvroSchema.Type.UNION =>
        val alternatives = avro.getTypes.asScala
        val alts = SortedMap.newBuilder[String, AvroSchema]
        alternatives.foreach { alt =>
          alts += (alt.getName -> alt)
        }
        UnionF(alts.result())

      case other => throw UnparseableAvroSchema(s"Unknown AvroSchema type: $other!")
    }
  }

  // These are internal properties that should be filtered out
  private val filter = Set(
    RECORD_TYPE_KEY,
    ORIGINAL_NAME_KEY,
    NO_HEADER
  )

  private def extractProps(avro: AvroSchema) = {
    val props = SortedMap.newBuilder[String, Property]
    avro.getObjectProps.asScala.foreach {
      case (k, v) =>
        if (!filter.contains(k)) {
          props += (k -> JacksonUtils.toJsonNode(v).toDatum)
        }
    }
    props.result()
  }

  private def originalName(field: AvroSchema.Field) = {
    if (field.schema().isUnion) {
      field.schema().getTypes.asScala.last.getProp(ORIGINAL_NAME_KEY) match {
        case null => field.name()
        case orig => orig
      }
    } else {
      field.schema().getProp(ORIGINAL_NAME_KEY) match {
        case null => field.name()
        case orig => orig
      }
    }
  }

  private val fromAvroSchemaFn = scheme.ana(coalgebra)

  def read(schema: AvroSchema): Schema = {
    fromAvroSchemaFn(schema)
  }

  def using(coalgebra: Coalgebra[SchemaF, AvroSchema]): AvroSchema => Schema = {
    val fn = scheme.ana(coalgebra)
    avro =>
      fn(avro)
  }
}
