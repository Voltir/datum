package datum.patterns.schemas

import cats.{Applicative, Traverse}
import cats.instances.sortedMap._
import cats.instances.vector._
import datum.patterns.properties.Property
import higherkindness.droste.util.DefaultTraverse

import scala.collection.immutable.SortedMap

sealed trait SchemaF[+R] extends Product with Serializable {
  def properties: PropertyMap
  def withProperties(Property: (String, Property)*): SchemaF[R]
}

final case class ObjF[R](
  fields: SortedMap[String, R],
  properties: PropertyMap = Map.empty
) extends SchemaF[R] {
  override def withProperties(additional: (String, Property)*): SchemaF[R] =
    ObjF(fields, properties ++ additional)
}

final case class RowF[R](
  columns: Vector[Column[R]],
  properties: PropertyMap = Map.empty
) extends SchemaF[R] {
  override def withProperties(additional: (String, Property)*): SchemaF[R] =
    RowF(columns, properties ++ additional)
}

final case class ArrayF[R](
  conforms: R,
  properties: PropertyMap = Map.empty
) extends SchemaF[R] {
  override def withProperties(additional: (String, Property)*): SchemaF[R] =
    ArrayF(conforms, properties ++ additional)
}

final case class UnionF[R](
  alternatives: SortedMap[String, R],
  properties: PropertyMap = Map.empty
) extends SchemaF[R] {
  override def withProperties(additional: (String, Property)*): SchemaF[R] =
    UnionF(alternatives, properties ++ additional)
}

final case class ValueF(
  tpe: Type,
  properties: PropertyMap = Map.empty
) extends SchemaF[Nothing] {
  override def withProperties(additional: (String, Property)*): SchemaF[Nothing] =
    ValueF(tpe, properties ++ additional)
}

object SchemaF {
  implicit val traverse: Traverse[SchemaF] = new DefaultTraverse[SchemaF] {
    override def traverse[G[_], A, B](fa: SchemaF[A])(f: A => G[B])(implicit G: Applicative[G]): G[SchemaF[B]] = {
      fa match {
        case v @ ValueF(_, _) => G.pure(v)

        case RowF(columns, props) =>
          val tl = Traverse[Vector].traverse(columns) { e =>
            G.map(f(e.value))(Column.apply(_, e.header))
          }
          G.map(tl)(x => RowF(x, props))

        case ObjF(fields, props) =>
          val tm = Traverse[SortedMap[String, *]].traverse(fields)(f)
          G.map(tm)(x => ObjF(x, props))

        case UnionF(alts, attrs) =>
          val tm = Traverse[SortedMap[String, *]].traverse(alts)(f)
          G.map(tm)(x => UnionF(x, attrs))

        case ArrayF(e, meta) => G.map(f(e))(x => ArrayF(x, meta))
      }
    }
  }
}
