package datum.algebras.prefix
import cats.data.Chain
import cats.instances.list._
import datum.patterns.schemas
import datum.patterns.schemas._
import higherkindness.droste.{AlgebraM, scheme}
import higherkindness.droste.data.{Attr, AttrF}
import higherkindness.droste.data.prelude._
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.SortedMap

class PrefixSpec extends WordSpec with Matchers {

  "The Prefix function" should {
    "work in a simple case" in {
      val schema = schemas.obj()()
      val check = Attr.apply[SchemaF, Prefix](Chain.one(PathPart.Root), ObjF(SortedMap.empty))
      Prefix(schema) shouldBe check
    }

    "PathPrefix should have a reasonable conversion to string" in {
      val prefix = Chain(PathPart.Root, PathPart.Field("A"), PathPart.Field("B"))
      Prefix.toString(prefix) shouldBe "/A/B"
    }

    "PathPrefix should convert arrays to strings" in {
      val prefix = Chain(PathPart.Root, PathPart.ArrayPart, PathPart.Field("B"))
      Prefix.toString(prefix) shouldBe "/(Array)/B"
    }

    "PathPrefix should handle union schemas" in {
      val schema = schemas.union()(
        "foo" -> schemas.obj()("ok" -> schemas.value(BooleanType)),
        "bar" -> schemas.value(IntType)
      )

      // collects a list of terminal prefixes
      val collect: AlgebraM[List, SchemaWithPrefixF, String] = AlgebraM[List, SchemaWithPrefixF, String] {
        case AttrF(_, UnionF(alts, _)) => alts.values.toList
        case AttrF(_, ObjF(fields, _))      => fields.values.toList
        case AttrF(p, _)                    => Prefix.toString(p) :: Nil
      }
      val fn = scheme.cataM(collect)

      // Generate a prefix tree for our schema
      val prefix = Prefix(schema)

      val result: List[String] = fn(prefix)

      result should contain("/{foo}/ok")
      result should contain("/{bar}")
    }
  }

}
