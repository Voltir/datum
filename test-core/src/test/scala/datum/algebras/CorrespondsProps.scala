package datum.algebras

import datum.algebras.corresponds.Corresponds
import datum.gen.algebras.{DataGen, SchemaGen}
import datum.gen.algebras.SchemaGen.{AnObj, Seed}
import datum.patterns.data.Data
import datum.patterns.schemas
import datum.patterns.schemas.{IntType, Schema, TextType}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.WordSpec
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers

class CorrespondsProps extends WordSpec with Checkers {

  private val genSchema = SchemaGen.define()(Seed(AnObj, 5))

  private val dataGenFn = DataGen.define(DataGen.algebra)

  val otherSchema: Schema = schemas.obj()(
    "foo" -> schemas.value(IntType),
    "bar" -> schemas.value(TextType)
  )

  val genDataFromSchema: Gen[(Schema, List[Data])] = {
    for {
      s <- genSchema
      d <- dataGenFn(s)
      ds <- Gen.listOf(d)
    } yield (s, ds)
  }

  implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary(genDataFromSchema)

  private val correspondsTo: Schema => Data => Boolean =
    Corresponds.define(Corresponds.optional(Corresponds.algebra))

  private val negFn: Data => Boolean = correspondsTo(otherSchema)

  "The correspondence function" should {
    "work for schema generated data" in {
      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, data) = generated
          val fn: Data => Boolean = correspondsTo(schema)
          data.forall(fn) && (data.isEmpty || !data.exists(negFn))
        }
      }
    }
  }

}
