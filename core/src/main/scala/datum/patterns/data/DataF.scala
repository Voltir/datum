package datum.patterns.data

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

import cats.{Applicative, Traverse}
import qq.droste.util.DefaultTraverse
import cats.instances.sortedMap._
import cats.instances.vector._
import cats.instances.string._

import scala.collection.immutable.SortedMap

sealed trait DataF[+R] extends Product with Serializable

final case class ObjValue[R](fields: SortedMap[String, R]) extends DataF[R]
final case class RowValue[R](values: Vector[R]) extends DataF[R]
final case class NamedUnionValue[R](selection: String, value: R) extends DataF[R]
final case class IndexedUnionValue[R](index: Int, value: R) extends DataF[R]

final case class IntValue(value: Int) extends DataF[Nothing]
final case class LongValue(value: Long) extends DataF[Nothing]
final case class FloatValue(value: Float) extends DataF[Nothing]
final case class DoubleValue(value: Double) extends DataF[Nothing]
final case class TextValue(value: String) extends DataF[Nothing]
final case class BooleanValue(value: Boolean) extends DataF[Nothing]
final case class BytesValue(value: Array[Byte]) extends DataF[Nothing]
final case class DateValue(value: LocalDate) extends DataF[Nothing]
final case class TimestampValue(value: Instant) extends DataF[Nothing]
final case class DateTimeValue(value: LocalDateTime) extends DataF[Nothing]
final case class ZonedTimeValue(value: ZonedDateTime) extends DataF[Nothing]
case object EmptyValue extends DataF[Nothing]

object DataF {
  implicit val traverse: Traverse[DataF] = new DefaultTraverse[DataF] {
    override def traverse[G[_], A, B](fa: DataF[A])(f: A => G[B])(implicit G: Applicative[G]): G[DataF[B]] = {
      fa match {
        case ObjValue(fields) =>
          val tm = Traverse[SortedMap[String, ?]].traverse(fields)(f)
          G.map(tm)(ObjValue.apply)

        case RowValue(vs) =>
          val tv = Traverse[Vector].traverse(vs)(f)
          G.map(tv)(RowValue.apply)

        case NamedUnionValue(selection, a) =>
          G.map(f(a))(b => NamedUnionValue(selection, b))

        case IndexedUnionValue(idx, a) =>
          G.map(f(a))(b => IndexedUnionValue(idx, b))

        case EmptyValue            => G.pure(EmptyValue)
        case v @ TextValue(_)      => G.pure(v)
        case v @ BooleanValue(_)   => G.pure(v)
        case v @ LongValue(_)      => G.pure(v)
        case v @ IntValue(_)       => G.pure(v)
        case v @ DoubleValue(_)    => G.pure(v)
        case v @ FloatValue(_)     => G.pure(v)
        case v @ BytesValue(_)     => G.pure(v)
        case v @ DateValue(_)      => G.pure(v)
        case v @ TimestampValue(_) => G.pure(v)
        case v @ DateTimeValue(_)  => G.pure(v)
        case v @ ZonedTimeValue(_) => G.pure(v)
      }
    }
  }
}
