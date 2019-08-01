package datum.ujsonlib.data

import java.util.Base64
import java.time.format.DateTimeFormatter
import java.time.ZoneId

import datum.patterns.{data, schemas}
import datum.patterns.data.{Data, DataF}
import datum.patterns.schemas._
import higherkindness.droste.data.Fix
import higherkindness.droste.{Algebra, scheme}

import scala.collection.mutable

object WriteJs {

  private def toJs(tpe: schemas.Type, d: DataF[Data]): ujson.Value = {
    (tpe, d) match {
      case (IntType, data.IntValue(v))         => ujson.Num(v)
      case (LongType, data.LongValue(v))       => ujson.Str(v.toString)
      case (FloatType, data.FloatValue(v))     => ujson.Num(v)
      case (DoubleType, data.DoubleValue(v))   => ujson.Num(v)
      case (TextType, data.TextValue(v))       => ujson.Str(v)
      case (BooleanType, data.BooleanValue(v)) => ujson.Bool(v)
      case (BytesType, data.BytesValue(v)) =>
        val encoded = Base64.getEncoder.encodeToString(v)
        ujson.Str(encoded)
      case (DateType, data.DateValue(v)) =>
        ujson.Str(v.format(DateTimeFormatter.ISO_LOCAL_DATE))
      case (TimestampType, data.TimestampValue(v)) =>
        ujson.Str(v.atZone(ZoneId.of("UTC")).format(DateTimeFormatter.ISO_INSTANT))
      case (DateTimeType, data.DateTimeValue(v)) =>
        ujson.Str(v.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      case (ZonedDateTimeType, data.ZonedTimeValue(v)) =>
        ujson.Str(v.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))
      case _ => ujson.Null
    }
  }

  val algebra: Algebra[SchemaF, Data => ujson.Value] = Algebra {
    case ValueF(tpe, _) =>
      d =>
        toJs(tpe, Fix.un[DataF](d))

    case ObjF(schema, _) =>
      Fix.un[DataF](_) match {
        case data.ObjValue(fields) =>
          val has = fields.keySet.intersect(schema.keySet)
          val missing = schema.keySet.diff(fields.keySet)
          val invalid = fields.keySet.diff(schema.keySet)

          if (invalid.nonEmpty) ujson.Null
          else {
            val jsFields = mutable.LinkedHashMap.empty[String, ujson.Value]
            has.foreach { k =>
              jsFields.put(k, schema(k)(fields(k)))
            }

            missing.foreach { k =>
              jsFields.put(k, schema(k)(data.empty))
            }

            ujson.Obj(jsFields)
          }

        case _ => ujson.Null
      }

    case RowF(elements, _) =>
      Fix.un[DataF](_) match {
        case data.RowValue(rows) =>
          val empty = Stream.continually(data.empty)
          elements.zip(rows.toStream #::: empty).map {
            case (col, d) => col.value.apply(d)
          }
        case _ => ujson.Null
      }

    case ArrayF(fn, _) =>
      Fix.un[DataF](_) match {
        case data.RowValue(values) => values.map(fn)
        case _                     => ujson.Null
      }

    case UnionF(alts, _) =>
      Fix.un[DataF](_) match {
        case data.NamedUnionValue(selection, value) =>
          alts
            .get(selection)
            .map { fn =>
              ujson.Obj(
                selection -> fn(value)
              )
            }
            .getOrElse(ujson.Null)
        case _ =>
          ujson.Null
      }

    case _ =>
      _ =>
        ujson.Null
  }

  def define(using: Algebra[SchemaF, Data => ujson.Value] = algebra): Schema => Data => ujson.Value = {
    scheme.cata(using)
  }
}
