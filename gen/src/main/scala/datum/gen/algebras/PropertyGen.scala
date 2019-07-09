package datum.gen.algebras

import datum.patterns.properties._
import higherkindness.droste.{CoalgebraM, scheme}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._

import scala.collection.immutable.SortedMap

object PropertyGen {

  sealed trait Next extends Serializable with Product
  case object ABoolProp extends Next
  case object AnIntProp extends Next
  case object AStringProp extends Next
  case object ACollectionProp extends Next

  case class Seed(next: Next, level: Int)

  private val genPrimitive: Gen[Next] = {
    Gen.frequency(
      2 -> Gen.const(ABoolProp),
      1 -> Gen.const(AnIntProp),
      1 -> Gen.const(AStringProp)
    )
  }

  private val genNext: Gen[Next] = Gen.frequency(
    1 -> genPrimitive,
    1 -> Gen.const(ACollectionProp)
  )

  private def nest(level: Int): Gen[Seed] = {
    val next =
      if (level > 0) genNext
      else genPrimitive

    for {
      n <- next
      l <- Gen.choose(0, Math.max(level - 1, 0))
    } yield Seed(n, l)
  }

  val coalgebra: CoalgebraM[Gen, PropertyF, Seed] = CoalgebraM[Gen, PropertyF, Seed] {
    case Seed(ABoolProp, _)   => arbitrary[Boolean].map(BoolPropF)
    case Seed(AnIntProp, _)   => arbitrary[Int].map(IntPropF)
    case Seed(AStringProp, _) => arbitrary[String].map(StringPropF)
    case Seed(ACollectionProp, level) =>
      val genProps = for {
        k <- Gen.resize(5, Gen.alphaLowerStr)
        s <- nest(level)
      } yield (k, s)

      Gen.resize(3, Gen.nonEmptyListOf(genProps).map { props =>
        CollectionPropertyF[Seed](SortedMap(props: _*))
      })
  }

  def define(level: Int = 2) = {
    val fn = scheme.anaM(coalgebra)
    fn(Seed(ACollectionProp, level))
  }
}
