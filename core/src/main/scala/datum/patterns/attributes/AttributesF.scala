//package datum.patterns.attributes
//
//import cats.{Applicative, Traverse}
//import cats.instances.vector._
//
//import higherkindness.droste.util.DefaultTraverse
//
//sealed trait AttributesF[+R] extends Product with Serializable
//
//case class Label[R](name: String, value: R) extends AttributesF[R]
//case class Collection[R](values: Vector[R]) extends AttributesF[R]
//case class Property(value: String) extends AttributesF[Nothing]
//case class NumProperty(value: Double) extends AttributesF[Nothing]
//case class BoolProperty(value: Boolean) extends AttributesF[Nothing]
//case object Flag extends AttributesF[Nothing]
//
//object AttributesF {
//  implicit val traverse: Traverse[AttributesF] = new DefaultTraverse[AttributesF] {
//    override def traverse[G[_], A, B](fa: AttributesF[A])(f: A => G[B])(
//      implicit G: Applicative[G]): G[AttributesF[B]] = {
//      fa match {
//        case v @ BoolProperty(_) => G.pure(v)
//        case v @ NumProperty(_)  => G.pure(v)
//        case v @ Property(_)     => G.pure(v)
//        case Flag                => G.pure(Flag)
//        case Label(n, a) =>
//          G.map(f(a)) { b =>
//            Label(n, b)
//          }
//        case Collection(vs) =>
//          val tv = Traverse[Vector].traverse(vs)(f)
//          G.map(tv)(Collection.apply)
//      }
//    }
//  }
//}
