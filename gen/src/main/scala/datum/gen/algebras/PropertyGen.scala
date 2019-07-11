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
  case object ANumProp extends Next
  case object ATextProp extends Next
  case object ACollectionProp extends Next

  case class Seed(next: Next, level: Int)

  private val genPrimitive: Gen[Next] = {
    Gen.frequency(
      2 -> Gen.const(ABoolProp),
      1 -> Gen.const(ANumProp),
      1 -> Gen.const(ATextProp)
    )
  }

  private val genNext: Gen[Next] = Gen.frequency(
    2 -> genPrimitive,
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
    case Seed(ABoolProp, _) => arbitrary[Boolean].map(BoolPropF)
    case Seed(ANumProp, _)  => arbitrary[Double].map(NumPropF)
    case Seed(ATextProp, _) => arbitrary[String].map(TextPropF)
    case Seed(ACollectionProp, level) =>
      val genProps = for {
        k <- Gen.resize(5, Gen.alphaLowerStr)
        s <- nest(level)
      } yield (k, s)

      Gen.resize(3, Gen.nonEmptyListOf(genProps).map { props =>
        CollectionPropF[Seed](SortedMap(props: _*))
      })
  }

  def define(level: Int = 1): Gen[(String, Property)] = {
    val key = Gen.resize(5, Gen.alphaLowerStr)
    val fn = scheme.anaM(coalgebra)
    val value = genNext.flatMap(n => fn(Seed(n, level)))
    for {
      k <- key
      v <- value
    } yield k -> v
  }
}
