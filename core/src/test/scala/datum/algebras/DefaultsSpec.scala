package datum.algebras

import datum.algebras.defaults._
import datum.patterns.{data, schemas}
import datum.patterns.schemas._
import datum.patterns.attributes._
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.Checkers
import datum.patterns.data.{Data, DataF, ObjValue, RowValue}
import qq.droste.data.Fix

class DefaultsSpec extends WordSpec with Checkers with Matchers {

  /* Constructs a "schema compiler" that has rules for taking
   * the schema's "default" Attributes and converting it into Data.
   */
  val compiler = CompileDefaults()

  "The Defaults algebra" should {
    "work given a basic example" in {
      val person: Schema = schemas.obj()(
        "name" -> schemas.value(TextType),
        "foo" -> schemas.value(BooleanType, defaults.use(property(true))),
        "age" -> schemas.value(IntType, defaults.use(property(42)))
      )

      val sample: Data = data.obj(
        "name" -> data.text("Bob")
      )

      // Annotates a Schema with "Data" values - this is the default value at that position.
      // The schema itself might be malformed, so this returns Either[Error, AnnotatedSchema].
      val compiled = compiler.compile(person)

      // Assume nothing wrong with the default definitions in the schema.
      val annotated = compiled.right.get

      // Given the annotated schema, we can generate a function from Data => Data,
      // applying defaults where input data is missing.
      val applyDefaults: Data => Data = ApplyDefaults.using(annotated)

      // And we can use that function on data
      val result = applyDefaults(sample)

      Fix.un[DataF](result) shouldBe a[ObjValue[_]]

      // downcast type and check values
      val check = Fix.un[DataF](result).asInstanceOf[ObjValue[Data]]

      check.fields.keySet should contain("foo")
      check.fields.keySet should contain("age")
      check.fields("age") shouldBe data.integer(42)
      check.fields("foo") shouldBe data.boolean(true)
    }

    "insert fields given an empty input data obj" in {
      val schema: Schema = schemas.obj()(
        "foo" -> schemas.value(TextType, defaults.use(property("hello"))),
        "bar" -> schemas.value(IntType, defaults.use(property(1))),
        "nested" -> schemas.obj()(
          "inner" -> schemas.value(BooleanType, defaults.use(property(false)))
        ),
        "nope" -> schemas.obj()("missing" -> schemas.value(TextType))
      )

      val annotated = compiler.compile(schema).right.get

      val fn = ApplyDefaults.using(annotated)

      val result = fn(data.empty)
      Fix.un[DataF](result) shouldBe a[ObjValue[_]]

      // downcast type and check values
      val check = Fix.un[DataF](result).asInstanceOf[ObjValue[Data]]

      check.fields.keySet should contain("foo")
      check.fields.keySet should contain("bar")
      check.fields.keySet should contain("nested")
      check.fields.keySet shouldNot contain("nope")

      check.fields("foo") shouldBe data.text("hello")
      check.fields("bar") shouldBe data.integer(1)
      check.fields("nested") shouldBe data.obj("inner" -> data.boolean(false))
    }

    "insert default fields for a Row" in {
      val person: Schema = schemas.row()(
        Column(schemas.value(TextType), Some("name")),
        Column(schemas.value(BooleanType, defaults.use(property(true))), Some("foo")),
        Column(schemas.value(IntType, defaults.use(property(42))), Some("age"))
      )

      val sample1: Data = data.row(
        data.text("Bob"),
        data.empty,
        data.empty
      )

      val annotated = compiler.compile(person)

      val fn = ApplyDefaults.using(annotated.right.get)
      val result = fn(sample1)

      Fix.un[DataF](result) shouldBe a[RowValue[_]]

      val check = Fix.un[DataF](result).asInstanceOf[RowValue[Data]]

      check.values should contain allOf (data.text("Bob"), data.boolean(true), data.integer(42))
    }

    "resize columns modifier should work" in {
      val person: Schema = schemas.row(Map(defaults.modifiers.EnableColumnDefaultExpansion.enable))(
        Column(schemas.value(TextType), Some("name")),
        Column(schemas.value(BooleanType, defaults.use(property(true))), Some("foo")),
        Column(schemas.value(IntType, defaults.use(property(42))), Some("age"))
      )

      val annotated = compiler.compile(person).right.get
      val fn = ApplyDefaults.using(annotated, defaults.modifiers.EnableColumnDefaultExpansion.algebra)

      val sample: Data = data.row(
        data.text("Bob"),
        data.empty
      )

      val r1 = Fix.un[DataF](fn(sample)).asInstanceOf[RowValue[Data]]
      val r2 = Fix.un[DataF](fn(data.empty)).asInstanceOf[RowValue[Data]]

      r1.values should contain allOf (data.text("Bob"), data.boolean(true), data.integer(42))
      r2.values should contain allOf (data.empty, data.boolean(true), data.integer(42))

    }

    "fail to compile an invalid obj schema" in {
      val schema: Schema = schemas.obj()("fail" -> schemas.value(IntType, defaults.use(property("not an int"))))
      compiler.compile(schema) shouldBe a[Left[_, _]]
    }

    "fail to compile an invalid row schema" in {
      val schema: Schema = schemas.row()(
        Column(schemas.value(IntType)),
        Column(schemas.value(IntType, defaults.use(property("not an int"))))
      )
      compiler.compile(schema) shouldBe a[Left[_, _]]
    }
  }
}
