package datum.ujsonlib
import datum.gen.algebras.{DataGen, SchemaGen}
import datum.gen.algebras.SchemaGen._
import datum.patterns.{data => d}
import datum.patterns.data.Data
import datum.patterns.schemas._
import datum.patterns.{schemas => s}
import datum.ujsonlib.data.{JsReader, WriteJs}
import datum.ujsonlib.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers
import cats.instances.all._
import datum.modifiers.Optional

class UjsonLibSpec extends WordSpec with Checkers with Matchers {

  private val aSeed = Gen.oneOf(SchemaGen.AnObj, SchemaGen.ARow, SchemaGen.AUnion).map(Seed(_, 5))

  private val generateUsing = DataGen.define()
  implicit def arbData(implicit schema: Schema): Arbitrary[Data] = Arbitrary {
    generateUsing(schema)
  }

  val reader = JsReader[Either[Throwable, ?]]

  "UJson Lib" should {
    "be able to encode/decode an arbitrary schema to json" in {
      implicit val arb: Arbitrary[Schema] = Arbitrary {
        val fn = SchemaGen.define(SchemaGen.optional(SchemaGen.default.coalgebra))
        aSeed.flatMap(fn)
      }

      check {
        forAll { schema: Schema =>
          val js = upickle.default.writeJs(schema)
          val parsed = upickle.default.read[Schema](js)
          parsed == schema
        }
      }
    }

    "roundtrip a simple obj" in {
      implicit val schema: Schema = s.obj()("a" -> s.value(IntType), "b" -> s.value(TextType))
      val toJs = WriteJs.define()(schema)
      val fromJs = reader.define(schema)

      check {
        forAll { data: Data =>
          fromJs(toJs(data)) == Right(data)
        }
      }
    }

    "union should work with rows" in {
      val schema = s.union()(
        "foo" -> s.row()(Column(s.value(BooleanType))),
        "bar" -> s.value(DateType),
        "baz" -> s.array()(s.value(LongType))
      )
      val toJs = WriteJs.define()(schema)
      val fromJs = reader.define(schema)
      val check = d.union("baz", d.row(d.long(1), d.long(2)))

      fromJs(toJs(check)) shouldBe Right(check)
    }

    "union should work with objs" in {
      val schema = s.union()(
        "foo" -> s.obj()("a" -> s.value(BooleanType)),
        "bar" -> s.obj()("b" -> s.value(BooleanType)),
      )
      val toJs = WriteJs.define()(schema)
      val fromJs = reader.define(schema)
      val check = d.union("bar", d.obj("b" -> d.boolean(true)))

      fromJs(toJs(check)) shouldBe Right(check)
    }

    "indexed should work with empty array" in {
      val schema = s.indexed()(
        s.value(BooleanType),
        s.array()(s.value(BooleanType))
      )
      val toJs = WriteJs.define()(schema)
      val fromJs = reader.define(schema)

      val c0 = d.indexed(0, d.boolean(true))
      val c1 = d.indexed(1, d.row())

      fromJs(toJs(c0)) shouldBe Right(c0)
      fromJs(toJs(c1)) shouldBe Right(c1)
    }

    "roundtrip binary data" in {
      implicit val schema: Schema = s.obj()("a" -> s.value(BytesType), "b" -> s.value(BytesType))
      val toJs = WriteJs.define()(schema)
      val fromJs = reader.define(schema)
      check {
        forAll { data: Data =>
          fromJs(toJs(data)) == Right(data)
        }
      }
    }

    "work with missing data" in {
      val schema: Schema = s.obj()("a" -> s.value(IntType, Optional.enable), "b" -> s.value(TextType, Optional.enable))

      val fromJs = reader.define(schema)
      val fromJsOpt = reader.defineUsing(reader.optional(reader.default))(schema)

      fromJs(ujson.Obj()) shouldBe a[Left[Throwable, Data]]
      fromJsOpt(ujson.Obj()) shouldBe Right(d.obj("a" -> d.empty, "b" -> d.empty))
    }

    "be able to encode/decode arbitrary data of some schema" in {
      val generator = SchemaGen.default

      implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary {
        for {
          seed <- aSeed
          schema <- SchemaGen.define(generator.coalgebra)(seed)
          data <- Gen.nonEmptyListOf(DataGen.define()(schema))
        } yield (schema, data)
      }

      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, data) = generated
          val toJs = WriteJs.define()(schema)
          val fromJs = reader.define(schema)

          data.map(d => fromJs(toJs(d))).forall(_.isRight)
        }
      }
    }
  }
}
