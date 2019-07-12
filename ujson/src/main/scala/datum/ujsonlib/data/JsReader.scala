package datum.ujsonlib.data

import scala.language.higherKinds
import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import java.util.Base64

import cats.{MonadError, Traverse}
import datum.patterns.data._
import datum.patterns.{schemas, data => d}
import datum.patterns.schemas._
import higherkindness.droste.{Algebra, scheme}
import cats.syntax.functor._
import cats.syntax.applicativeError._
import cats.instances.vector._
import cats.data.Chain
import datum.modifiers.Optional

import scala.collection.immutable.SortedMap

class JsReader[M[_]]()(implicit M: MonadError[M, Throwable]) {
  import JsReader._

  private def fromJs(tpe: schemas.Type, js: ujson.Value): M[Data] = {
    (tpe, js) match {
      case (IntType, ujson.Num(v))      => M.pure(d.integer(v.toInt))
      case (LongType, ujson.Str(v))     => M.catchNonFatal(d.long(v.toLong))
      case (FloatType, ujson.Num(v))    => M.pure(d.float(v.toFloat))
      case (DoubleType, ujson.Num(v))   => M.pure(d.double(v))
      case (TextType, ujson.Str(v))     => M.pure(d.text(v))
      case (BooleanType, ujson.Bool(v)) => M.pure(d.boolean(v))

      case (BytesType, ujson.Str(v)) =>
        M.catchNonFatal {
          d.bytes(Base64.getDecoder.decode(v))
        }

      case (DateType, ujson.Str(v)) =>
        M.catchNonFatal {
          d.date(LocalDate.parse(v))
        }

      case (TimestampType, ujson.Str(v)) =>
        M.catchNonFatal {
          d.timestamp(Instant.parse(v))
        }

      case (DateTimeType, ujson.Str(v)) =>
        M.catchNonFatal {
          d.localTime(LocalDateTime.parse(v))
        }

      case (ZonedDateTimeType, ujson.Str(v)) =>
        M.catchNonFatal {
          d.zonedTime(ZonedDateTime.parse(v))
        }

      case _ =>
        M.raiseError(JsTypeException(tpe, js))
    }
  }

  val default: Algebra[SchemaF, ujson.Value => M[Data]] = Algebra {
    case ValueF(tpe, _) =>
      js =>
        fromJs(tpe, js)

    case ObjF(schema, _) => {
      case ujson.Obj(fields) =>
        val values = schema.foldLeft(Chain.empty[M[(String, Data)]]) {
          case (acc, (k, fn)) =>
            acc.prepend(fn(fields.getOrElse(k, ujson.Null)).map((k, _)))
        }
        Traverse[Chain].sequence(values).map { collected =>
          val builder = SortedMap.newBuilder[String, Data]
          collected.iterator.foreach { case (k, d) => builder += k -> d }
          d.obj(builder.result())
        }

      case _ => M.raiseError(SchemaMismatchException("Invalid Obj"))
    }

    case RowF(elements, _) => {
      case ujson.Arr(values) =>
        val valuesThenNulls = elements.zip(values.toStream #::: Stream.continually[ujson.Value](ujson.Null))
        val results = Traverse[Vector].traverse(valuesThenNulls) {
          case (col, v) =>
            col.value.apply(v)
        }
        results.map(d.row)

      case _ => M.raiseError(SchemaMismatchException("Invalid Row"))
    }

    case ArrayF(fn, _) => {
      case ujson.Arr(values) =>
        values.toVector.map(fn)
        val wat = Traverse[Vector].traverse(values.toVector) { v =>
          fn(v)
        }
        M.map(wat)(d.row)
        Traverse[Vector].traverse(values.toVector)(fn).map(d.row)
      case _ =>
        M.raiseError(SchemaMismatchException("Invalid Array"))
    }

    case NamedUnionF(alts, _) => {
      case ujson.Obj(fields) =>
        val selection = fields.keySet.head
        alts(selection)(fields(selection)).map { res =>
          d.union(selection, res)
        }
      case _ => M.raiseError(SchemaMismatchException("Invalid Union Value"))
    }

    case IndexedUnionF(alts, _) => {
      case ujson.Arr(values) if values.length == 2 =>
        try {
          val idx = values(0).num.toInt
          val fn = alts(idx)
          fn(values(1)).map { res =>
            d.indexed(idx, res)
          }
        } catch {
          case e: Exception => M.raiseError(e)
        }
      case _ => M.raiseError(SchemaMismatchException("Invalid IndexedUnion Value"))
    }
  }

  def optional(alg: Algebra[SchemaF, ujson.Value => M[Data]]): Algebra[SchemaF, ujson.Value => M[Data]] =
    Algebra { schema => js =>
      val fn = alg(schema)
      val isOpt = schema.properties.get(Optional.key).contains(true)
      (isOpt, js.isNull) match {
        case (true, true)  => M.pure(d.empty)
        case (true, false) => fn(js).recover { case _ => d.empty }
        case (false, _)    => fn(js)
      }
    }

  def define(schema: Schema): ujson.Value => M[Data] = defineUsing(default)(schema)

  def defineUsing(
    algebra: Algebra[SchemaF, ujson.Value => M[Data]]
  ): Schema => ujson.Value => M[Data] = scheme.cata(algebra)

}

object JsReader {

  sealed abstract class JsReaderException(msg: String) extends Exception(msg)

  case class JsTypeException(tpe: Type, js: ujson.Value)
    extends JsReaderException(s"Expected ${schemas.Type.asString(tpe)} but got ${pprint.apply(js, 2)}")

  case class SchemaMismatchException(msg: String) extends JsReaderException(msg)

  def apply[M[_]](implicit M: MonadError[M, Throwable]): JsReader[M] = new JsReader[M]()

}
