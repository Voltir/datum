package datum.avrolib.schemas

import cats.data.State
import datum.modifiers.Optional
import datum.patterns.schemas._
import datum.patterns.properties._
import higherkindness.droste.{RAlgebraM, scheme}
import org.apache.avro.{LogicalType, LogicalTypes, SchemaBuilder, Schema => AvroSchema}

import collection.JavaConverters._
import scala.collection.mutable

object AvroSchemaWriter2 {

  val algebra: RAlgebraM[Schema, Registry, SchemaF, AvroSchema] = RAlgebraM[Schema, Registry, SchemaF, AvroSchema] {
    case ValueF(IntType, props) => primitive(AvroSchema.Type.INT, props)

    case ObjF(fields, props) =>
      val fingerprint = fields.hashCode()
      State { registry =>

        val seen = mutable.Map.empty[String, Int].withDefaultValue(0)
        val avro = fields
          .foldLeft(SchemaBuilder.record("r%x".format(fingerprint)).fields()) {
            case (acc, (k, v)) =>
              val (schema, record) = v
              schema
            val (name, modified) = uniqueName(k, seen)
              assert(!record.isUnion)
            if (modified) {
              record.addProp(ORIGINAL_NAME_KEY, k)
            }
//            acc.name(name).`type`(v).noDefault()

              ???
          }
          .endRecord()

        ???
      }
    case _ => ???
  }

  private def primitive(typ: AvroSchema.Type, props: PropertyMap): Registry[AvroSchema] = {
    State.pure {
      val result = AvroSchema.create(typ)
      //includeProps(result, props)
      ???
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

  // Helper function to ensure unique record keys, makes use of the ORIGINAL_NAME_KEY
  // avro property to remember what the original name was
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

  def optional(
    algebra: RAlgebraM[Schema, Registry, SchemaF, AvroSchema]
  ): RAlgebraM[Schema, Registry, SchemaF, AvroSchema] =
    RAlgebraM[Schema, Registry, SchemaF, AvroSchema] {

      case ObjF(fields, props) =>
        val fingerprint = fields.hashCode()
        State { registry =>
          val avro = fields
            .foldLeft(SchemaBuilder.record("r%x".format(fingerprint)).fields()) {
              case (acc, (k, v)) =>
                val (schema, avroSchema) = v
                schema match {
                  case ValueF(_, p) if p.get(Optional.key).contains(true.prop) =>
                    SchemaBuilder.nullable().`type`(avroSchema)
                  case ObjF(_, p) if p.get(Optional.key).contains(true.prop) =>
                    SchemaBuilder.nullable().`type`(avroSchema)
                  case UnionF(_, p) if p.get(Optional.key).contains(true.prop) =>
                    SchemaBuilder.nullable().`type`(avroSchema)
                  case _ =>
                    assert(false, "TODO OPTION2")
                    ???
                }
                if (schema.properties.get(Optional.key).contains(true.prop)) {}
                //            val (name, modified) = uniqueName(k, seen)
                //            if (modified) {
                //              safeAddProp(v, ORIGINAL_NAME_KEY, k)
                //            }
                //            acc.name(name).`type`(v).noDefault()

                ???
            }
            .endRecord()

          ???
        }
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

  def using(algebra: RAlgebraM[Schema, Registry, SchemaF, AvroSchema]): Schema => AvroSchema = { schema =>
    val fn = scheme.zoo.paraM(algebra)
    fn(schema).run(Map.empty).value._2
  }

}
