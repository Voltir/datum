package datum.patterns.schemas

import datum.patterns.attributes.{Attribute, AttributeMap}
import cats.{Applicative, Traverse}
import cats.instances.sortedMap._
import cats.instances.string._
import cats.instances.vector._
import qq.droste.util.DefaultTraverse

import scala.collection.immutable.SortedMap

sealed trait SchemaF[+R] extends Product with Serializable {
  def attributes: AttributeMap
  def withAttributes(attribute: (String, Attribute)*): SchemaF[R]
}

final case class ObjF[R](
  fields: SortedMap[String, R],
  attributes: AttributeMap = Map.empty
) extends SchemaF[R] {
  override def withAttributes(additional: (String, Attribute)*): SchemaF[R] =
    ObjF(fields, attributes ++ additional)
}

final case class RowF[R](
  elements: Vector[Column[R]],
  attributes: AttributeMap = Map.empty
) extends SchemaF[R] {
  override def withAttributes(additional: (String, Attribute)*): SchemaF[R] =
    RowF(elements, attributes ++ additional)
}

final case class ArrayF[R](
  conforms: R,
  attributes: AttributeMap = Map.empty
) extends SchemaF[R] {
  override def withAttributes(additional: (String, Attribute)*): SchemaF[R] =
    ArrayF(conforms, attributes ++ additional)
}

final case class NamedUnionF[R](
  alternatives: SortedMap[String, R],
  attributes: AttributeMap = Map.empty
) extends SchemaF[R] {
  override def withAttributes(additional: (String, Attribute)*): SchemaF[R] =
    NamedUnionF(alternatives, attributes ++ additional)
}

final case class IndexedUnionF[R](
  alternatives: Vector[R],
  attributes: AttributeMap = Map.empty
) extends SchemaF[R] {
  override def withAttributes(additional: (String, Attribute)*): SchemaF[R] =
    IndexedUnionF(alternatives, attributes ++ additional)
}

final case class ValueF(
  tpe: Type,
  attributes: AttributeMap = Map.empty
) extends SchemaF[Nothing] {
  override def withAttributes(additional: (String, Attribute)*): SchemaF[Nothing] =
    ValueF(tpe, attributes ++ additional)
}

object SchemaF {
  implicit val traverse: Traverse[SchemaF] = new DefaultTraverse[SchemaF] {
    override def traverse[G[_], A, B](fa: SchemaF[A])(f: A => G[B])(implicit G: Applicative[G]): G[SchemaF[B]] = {
      fa match {
        case v @ ValueF(_, _) => G.pure(v)

        case RowF(elems, attrs) =>
          val tl = Traverse[Vector].traverse(elems) { e =>
            G.map(f(e.value))(Column.apply(_, e.header))
          }
          G.map(tl)(x => RowF(x, attrs))

        case ObjF(fields, attrs) =>
          val tm = Traverse[SortedMap[String, ?]].traverse(fields)(f)
          G.map(tm)(x => ObjF(x, attrs))

        case NamedUnionF(alts, attrs) =>
          val tm = Traverse[SortedMap[String, ?]].traverse(alts)(f)
          G.map(tm)(x => NamedUnionF(x, attrs))

        case IndexedUnionF(alts, attrs) =>
          val tv = Traverse[Vector].traverse(alts)(f)
          G.map(tv)(x => IndexedUnionF(x, attrs))

        case ArrayF(e, meta) => G.map(f(e))(x => ArrayF(x, meta))
      }
    }
  }
}
