package datum.patterns.properties
import cats.{Applicative, Traverse}
import cats.instances.string._
import cats.instances.sortedMap._

import higherkindness.droste.util.DefaultTraverse

import scala.collection.immutable.SortedMap

sealed trait PropertyF[+R] extends Product with Serializable
case class BoolPropF(value: Boolean) extends PropertyF[Nothing]
case class NumPropF(value: Double) extends PropertyF[Nothing]
case class TextPropF(value: String) extends PropertyF[Nothing]
case class CollectionPropF[R](properties: SortedMap[String, R]) extends PropertyF[R]

object PropertyF {

  implicit val traverse: Traverse[PropertyF] = new DefaultTraverse[PropertyF] {
    override def traverse[G[_], A, B](fa: PropertyF[A])(f: A => G[B])(implicit G: Applicative[G]): G[PropertyF[B]] = {
      fa match {
        case p @ BoolPropF(_) => G.pure(p)
        case p @ NumPropF(_)  => G.pure(p)
        case p @ TextPropF(_) => G.pure(p)
        case CollectionPropF(properties) =>
          val traversed = Traverse[SortedMap[String, ?]].traverse(properties)(f)
          G.map(traversed)(CollectionPropF.apply)
      }
    }
  }
}
