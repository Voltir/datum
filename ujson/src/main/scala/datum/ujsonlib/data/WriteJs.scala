package datum.ujsonlib.data

import java.util.Base64
import java.time.format.DateTimeFormatter
import java.time.ZoneId

import datum.patterns.{data, schemas}
import datum.patterns.data.{Data, DataF}
import datum.patterns.schemas._
import qq.droste.{Algebra, scheme}
import qq.droste.syntax.project._
import ujson.Js

import scala.collection.mutable

object WriteJs {

  private def toJs(tpe: schemas.Type, d: DataF[Data]): Js.Value = {
    (tpe, d) match {
      case (IntType, data.IntValue(v))         => Js.Num(v)
      case (LongType, data.LongValue(v))       => Js.Str(v.toString)
      case (FloatType, data.FloatValue(v))     => Js.Num(v)
      case (DoubleType, data.DoubleValue(v))   => Js.Num(v)
      case (TextType, data.TextValue(v))       => Js.Str(v)
      case (BooleanType, data.BooleanValue(v)) => Js.Bool(v)
      case (BytesType, data.BytesValue(v)) =>
        val encoded = Base64.getEncoder.encodeToString(v)
        Js.Str(encoded)
      case (DateType, data.DateValue(v)) =>
        Js.Str(v.format(DateTimeFormatter.ISO_LOCAL_DATE))
      case (TimestampType, data.TimestampValue(v)) =>
        Js.Str(v.atZone(ZoneId.of("UTC")).format(DateTimeFormatter.ISO_INSTANT))
      case (DateTimeType, data.DateTimeValue(v)) =>
        Js.Str(v.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      case (ZonedDateTimeType, data.ZonedTimeValue(v)) =>
        Js.Str(v.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))
      case _ => Js.Null
    }
  }

  val algebra: Algebra[SchemaF, Data => Js.Value] = Algebra {
    case ValueF(tpe, _) =>
      d =>
        toJs(tpe, d.project)

    case ObjF(schema, _) =>
      _.project match {
        case data.ObjValue(fields) =>
          val has = fields.keySet.intersect(schema.keySet)
          val missing = schema.keySet.diff(fields.keySet)
          val invalid = fields.keySet.diff(schema.keySet)

          if (invalid.nonEmpty) Js.Null
          else {
            val jsFields = mutable.LinkedHashMap.empty[String, Js.Value]
            has.foreach { k =>
              jsFields.put(k, schema(k)(fields(k)))
            }

            missing.foreach { k =>
              jsFields.put(k, schema(k)(data.empty))
            }

            Js.Obj(jsFields)
          }

        case _ => Js.Null
      }

    case RowF(elements, _) =>
      _.project match {
        case data.RowValue(rows) =>
          val empty = Stream.continually(data.empty)
          elements.zip(rows.toStream #::: empty).map {
            case (col, d) => col.value.apply(d)
          }
        case _ => Js.Null
      }

    case ArrayF(fn, _) =>
      _.project match {
        case data.RowValue(values) => values.map(fn)
        case _                     => Js.Null
      }

    case NamedUnionF(alts, _) =>
      _.project match {
        case data.NamedUnionValue(selection, value) =>
          alts
            .get(selection)
            .map { fn =>
              Js.Obj(
                selection -> fn(value)
              )
            }
            .getOrElse(Js.Null)
        case _ =>
          Js.Null
      }

    case IndexedUnionF(alts, _) =>
      _.project match {
        case data.IndexedUnionValue(idx, v) if idx >= 0 && idx < alts.length =>
          val fn = alts(idx)
          Js.Arr(Js.Num(idx), fn(v))
      }

    case _ =>
      _ =>
        Js.Null
  }

  def define(using: Algebra[SchemaF, Data => Js.Value] = algebra): Schema => Data => Js.Value = {
    scheme.cata(using)
  }
}
