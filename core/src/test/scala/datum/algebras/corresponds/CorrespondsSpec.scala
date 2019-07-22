package datum.algebras.corresponds

import datum.modifiers.Optional
import datum.patterns.data.Data
import datum.patterns.schemas.{IntType, Schema, TextType}
import datum.patterns.{data, schemas}
import org.scalatest.{Matchers, WordSpec}

class CorrespondsSpec extends WordSpec with Matchers {

  val correspondsTo = Corresponds.define(Corresponds.optional(Corresponds.algebra))

  val otherSchema: Schema = schemas.obj()(
    "foo" -> schemas.value(IntType),
    "bar" -> schemas.value(TextType)
  )

  val negFn: Data => Boolean = correspondsTo(otherSchema)

  "The correspondence function" should {

    "work with optional value" in {

      val opt = schemas.obj()(
        "required" -> schemas.value(IntType),
        "maybe" -> schemas.value(IntType, Optional.enable)
      )

      val checkFn = correspondsTo(opt)

      val d1 = data.obj(
        "required" -> data.integer(1),
        "maybe" -> data.integer(1)
      )

      val d2 = data.obj(
        "required" -> data.integer(2),
        "maybe" -> data.empty
      )

      val d3 = data.obj(
        "required" -> data.integer(3)
      )

      val d4 = data.obj("nope" -> data.integer(4))

      checkFn(d1) shouldBe true
      checkFn(d2) shouldBe true
      checkFn(d3) shouldBe true
      checkFn(d4) shouldBe false
    }
  }
}
