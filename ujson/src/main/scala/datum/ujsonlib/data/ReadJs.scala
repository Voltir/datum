package datum.ujsonlib.data
import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import java.util.Base64

import cats.Traverse
import datum.patterns.data._
import datum.patterns.{schemas, data => d}
import datum.patterns.schemas._
import datum.patterns.properties._
import higherkindness.droste.{Algebra, scheme}
import cats.syntax.either._
import cats.instances.either._
import cats.instances.vector._
import cats.data.Chain
import datum.modifiers.Optional

import scala.collection.immutable.SortedMap

object ReadJs {

  private def fromJs(tpe: schemas.Type, js: ujson.Value): Either[String, Data] = {
    (tpe, js) match {
      case (IntType, ujson.Num(v))      => Right(d.integer(v.toInt))
      case (LongType, ujson.Str(v))     => Either.catchNonFatal(d.long(v.toLong)).leftMap(_ => "Invalid Long")
      case (FloatType, ujson.Num(v))    => Right(d.float(v.toFloat))
      case (DoubleType, ujson.Num(v))   => Right(d.double(v))
      case (TextType, ujson.Str(v))     => Right(d.text(v))
      case (BooleanType, ujson.Bool(v)) => Right(d.boolean(v))

      case (BytesType, ujson.Str(v)) =>
        Either
          .catchNonFatal {
            d.bytes(Base64.getDecoder.decode(v))
          }
          .leftMap(_ => "Invalid Bytes")

      case (DateType, ujson.Str(v)) =>
        Either
          .catchNonFatal {
            d.date(LocalDate.parse(v))
          }
          .leftMap(_ => "Invalid DateType")

      case (TimestampType, ujson.Str(v)) =>
        Either
          .catchNonFatal {
            d.timestamp(Instant.parse(v))
          }
          .leftMap(_ => "Invalid Timestamp")

      case (DateTimeType, ujson.Str(v)) =>
        Either
          .catchNonFatal {
            d.localTime(LocalDateTime.parse(v))
          }
          .leftMap(_ => "Invalid DateTime")

      case (ZonedDateTimeType, ujson.Str(v)) =>
        Either
          .catchNonFatal {
            d.zonedTime(ZonedDateTime.parse(v))
          }
          .leftMap(_ => "Invalid ZonedDateTime")

      case err => Left(s"Expected: ${schemas.Type.asString(tpe)} but got ${pprint.apply(err, 2)}")
    }
  }

  val algebra: Algebra[SchemaF, ujson.Value => Either[String, Data]] = Algebra {
    case ValueF(tpe, _) =>
      js =>
        fromJs(tpe, js)

    case ObjF(schema, _) => {
      case ujson.Obj(fields) =>
        val values = schema.foldLeft(Chain.empty[Either[String, (String, Data)]]) {
          case (acc, (k, fn)) =>
            acc.prepend(fn(fields.getOrElse(k, ujson.Null)).map((k, _)))
        }
        Traverse[Chain].sequence(values).map { collected =>
          val builder = SortedMap.newBuilder[String, Data]
          collected.iterator.foreach { case (k, d) => builder += k -> d }
          d.obj(builder.result())
        }

      case _ => Left("Invalid Obj")
    }

    case RowF(elements, _) => {
      case ujson.Arr(values) =>
        val valuesThenNulls = elements.zip(values.toStream #::: Stream.continually[ujson.Value](ujson.Null))
        val results = Traverse[Vector].traverse(valuesThenNulls) {
          case (col, v) =>
            col.value.apply(v)
        }
        results.map(d.row)

      case _ => Left("Invalid Row")
    }

    case ArrayF(fn, _) => {
      case ujson.Arr(values) =>
        Traverse[Vector].traverse(values.toVector)(fn).map(d.row)
      case _ =>
        Left("Invalid Array")
    }

    case NamedUnionF(alts, _) => {
      case ujson.Obj(fields) =>
        val selection = fields.keySet.head
        alts(selection)(fields(selection)).map { res =>
          d.union(selection, res)
        }
      case _ => Left("Invalid Union Value")
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
          case e: Exception => Left(s"Invalid Indexed Union: ${e.getMessage}")
        }
      case _ =>
        Left("Invalid IndexedUnion Value")
    }
  }

  def optional(
    alg: Algebra[SchemaF, ujson.Value => Either[String, Data]]
  ): Algebra[SchemaF, ujson.Value => Either[String, Data]] = Algebra { schema => js =>
    val fn = alg(schema)
    fn(js) match {
      case Left(_) if schema.properties.get(Optional.key).contains(true.prop) => Right(d.empty)
      case otherwise                                                          => otherwise
    }
  }

  def define(
    alg: Algebra[SchemaF, ujson.Value => Either[String, Data]] = algebra
  ): Schema => ujson.Value => Either[String, Data] = {
    scheme.cata(alg)
  }
}
