package datum.avrolib.schemas
import cats.data.State
import datum.patterns.schemas._
import datum.avrolib.properties._
import datum.modifiers.Optional
import datum.patterns.properties._
import higherkindness.droste.{AlgebraM, scheme}
import org.apache.avro.{LogicalType, LogicalTypes, SchemaBuilder, Schema => AvroSchema}

import scala.collection.mutable
import collection.JavaConverters._

object AvroSchemaWriter {

  def includeProps(avro: AvroSchema, props: PropertyMap): AvroSchema = {
    assert(!avro.isUnion, "Tried to includeProps on a union!")
    props.filterNot(_._1 == AVRO_LOGICAL_TYPE).foreach {
      case (k, prop) =>
        avro.addProp(k, prop.toAvro)
    }
    avro
  }

  val algebra: AlgebraM[Registry, SchemaF, AvroSchema] = AlgebraM {
    case ValueF(IntType, props)     => primitive(AvroSchema.Type.INT, props)
    case ValueF(LongType, props)    => primitive(AvroSchema.Type.LONG, props)
    case ValueF(FloatType, props)   => primitive(AvroSchema.Type.FLOAT, props)
    case ValueF(DoubleType, props)  => primitive(AvroSchema.Type.DOUBLE, props)
    case ValueF(BooleanType, props) => primitive(AvroSchema.Type.BOOLEAN, props)
    case ValueF(TextType, props)    => primitive(AvroSchema.Type.STRING, props)
    case ValueF(BytesType, props)   => primitive(AvroSchema.Type.BYTES, props)

    case ValueF(DateType, props) => logical(AvroSchema.Type.INT, LogicalTypes.date(), props)

    case ValueF(TimestampType, props) =>
      if (props.get(AVRO_LOGICAL_TYPE).contains("timestamp-millis".prop)) {
        logical(AvroSchema.Type.LONG, LogicalTypes.timestampMillis(), props)
      } else {
        logical(AvroSchema.Type.LONG, LogicalTypes.timestampMicros(), props)
      }

    case ValueF(DateTimeType, props) =>
      primitive(AvroSchema.Type.STRING, props).map { avro =>
        avro.addProp(RECORD_TYPE_KEY, "date-time")
        avro
      }

    case ValueF(ZonedDateTimeType, props) =>
      primitive(AvroSchema.Type.STRING, props).map { avro =>
        avro.addProp(RECORD_TYPE_KEY, "zoned-date-time")
        avro
      }

    case ObjF(fields, props) =>
      val fingerprint = fields.hashCode()
      State { registry =>
        if (registry.contains(fingerprint)) {
          (registry, registry(fingerprint))
        } else {
          val seen = mutable.Map.empty[String, Int].withDefaultValue(0)
          val avro = fields
            .foldLeft(SchemaBuilder.record("r%x".format(fingerprint)).fields()) {
              case (acc, (k, v)) =>
                val (name, modified) = uniqueName(k, seen)
                if (modified) {
                  safeAddProp(v, ORIGINAL_NAME_KEY, k)
                }
                acc.name(name).`type`(v).noDefault()
            }
            .endRecord()
          includeProps(avro, props)
          avro.addProp(RECORD_TYPE_KEY, "obj")
          (registry + (fingerprint -> avro), avro)
        }
      }

    case RowF(elements, props) =>
      val fingerprint = elements.hashCode()
      State { registry =>
        if (registry.contains(fingerprint)) {
          (registry, registry(fingerprint))
        } else {
          val seen = mutable.Map.empty[String, Int].withDefaultValue(0)
          val avro = elements
            .foldLeft(SchemaBuilder.record("r%x".format(fingerprint)).fields()) {
              case (acc, col) =>
                val original = col.header.getOrElse("")
                val (name, modified) = uniqueName(original, seen)

                if (modified) {
                  safeAddProp(col.value, ORIGINAL_NAME_KEY, original)
                }
                if (col.header.isEmpty) {
                  safeAddProp(col.value, NO_HEADER, true)
                }
                acc.name(name).`type`(col.value).noDefault()
            }
            .endRecord()
          includeProps(avro, props)
          avro.addProp(RECORD_TYPE_KEY, "row")
          (registry + (fingerprint -> avro), avro)
        }
      }

    case ArrayF(conforms, props) =>
      State.pure {
        val avro = AvroSchema.createArray(conforms)
        includeProps(avro, props)
        avro
      }

    case UnionF(alts, props) =>
      val fingerprint = alts.hashCode()
      State { registry =>
        if (registry.contains(fingerprint)) {
          (registry, registry(fingerprint))
        } else {
          val buffer = mutable.Buffer.empty[AvroSchema]
          alts.zipWithIndex.foreach {
            case ((k, alt), idx) =>
              val record = SchemaBuilder
                .record("r%x_%d".format(fingerprint, idx))
                .fields()
                .name("schema")
                .`type`(alt)
                .noDefault()
                .endRecord()
              record.addProp(ORIGINAL_NAME_KEY, k)
              buffer.append(record)
          }
          val avroUnion = AvroSchema.createUnion(buffer.asJava)
          val avro =
            SchemaBuilder
              .record("r%x".format(fingerprint))
              .fields()
              .name("union")
              .`type`(avroUnion)
              .noDefault()
              .endRecord()
          avro.addProp(RECORD_TYPE_KEY, "union")
          includeProps(avro, props)
          (registry + (fingerprint -> avro), avro)
        }
      }
  }

  private def primitive(typ: AvroSchema.Type, props: PropertyMap): Registry[AvroSchema] = {
    State.pure {
      val result = AvroSchema.create(typ)
      includeProps(result, props)
    }
  }

  private def logical(typ: AvroSchema.Type, logical: LogicalType, props: PropertyMap): Registry[AvroSchema] = {
    State.pure {
      val result = AvroSchema.create(typ)
      logical.addToSchema(result)
      includeProps(result, props)
    }
  }

  // Helper function to ensure only valid (no special characters, no duplicates, etc..) names
  private def safeName(inp: String): (String, Boolean) = {
    val builder = mutable.StringBuilder.newBuilder
    var modified = false

    inp.iterator.foreach {
      case c if c.isLetterOrDigit || c == '_' =>
        builder.append(c)

      case _ =>
        builder.append('_')
        modified = true
    }

    val result = builder.result()
    if (result == "") ("Unnamed", true)
    else if (!(result.head.isLetter || result.head == '_')) (s"_$result", true)
    else (result, modified)
  }

  private def uniqueName(original: String, seen: mutable.Map[String, Int]): (String, Boolean) = {
    def next(check: String): String = {
      val result =
        if (seen(check) == 0) check
        else next(s"$check${seen(check) + 1}")
      seen(check) += 1
      result
    }

    val (safe, modified) = safeName(original)

    if (seen(safe) == 0) {
      seen(safe) += 1
      (safe, modified)
    } else {
      (next(safe), true)
    }
  }

  // This uses the convention that a fake record is appended to the end of a union to carry
  // (datum) properties around
  private def safeAddProp(field: AvroSchema, key: String, prop: Any): AvroSchema = {
    if (field.isUnion) {
      // This is really annoying, union types can come from both a union based schema
      // OR from optional values. This unfortunately special cases detecting an optional
      // value, which is represented as a union of the original schema and null
      //
      // I think to avoid this, we would have to go to .para instead of .cata
      val types = field.getTypes
      if (types.size() == 2 && types.get(1).isNullable) {
        safeAddProp(types.get(0), key, prop)
      } else if (types.size() == 2 && types.get(0).isNullable) {
        safeAddProp(types.get(1), key, prop)
      } else {
        assert(false, "All unions should be wrapped at this point!")
      }
    } else {
      field.addProp(key, prop)
    }
    field
  }

  def write(schema: Schema): AvroSchema = {
    using(optional(algebra))(schema)
  }

  def optional(algebra: AlgebraM[Registry, SchemaF, AvroSchema]): AlgebraM[Registry, SchemaF, AvroSchema] =
    AlgebraM[Registry, SchemaF, AvroSchema] {
      case x if x.properties.get(Optional.key).contains(true.prop) =>
        algebra(x).map { generic =>
          if (!generic.isUnion) {
            SchemaBuilder.nullable().`type`(generic)
          } else {
            val alts = generic.getTypes.asScala
            alts.prepend(AvroSchema.create(AvroSchema.Type.NULL))
            AvroSchema.createUnion(alts.asJava)
          }
        }
      case otherwise => algebra(otherwise)
    }

  def using(algebra: AlgebraM[Registry, SchemaF, AvroSchema]): Schema => AvroSchema = {
    val fn = scheme.cataM(algebra)
    schema =>
      fn(schema).run(Map.empty).value._2
  }
}
