package datum.ujsonlib.schemas

import datum.ujsonlib.attributes.AttributeReadWriter
import datum.patterns.attributes.Attribute
import datum.patterns.schemas._
import qq.droste.{Algebra, Coalgebra, scheme}
import ujson.Js
import upickle.default._

import scala.collection.immutable.SortedMap

trait SchemaReadWriter { self: AttributeReadWriter =>

  val algebra: Algebra[SchemaF, Js.Value] = Algebra {
    case ObjF(fields, attributes) =>
      Js.Obj(
        "fields" -> fields,
        "attributes" -> writeJs(attributes)
      )

    case RowF(columns, attributes) =>
      Js.Obj(
        "columns" ->
          columns.map(
            col =>
              col.header.map { hdr =>
                Js.Obj("schema" -> col.value, "header" -> hdr)
              } getOrElse {
                Js.Obj("schema" -> col.value)
            }
          ),
        "attributes" -> writeJs(attributes)
      )

    case ArrayF(element, attributes) =>
      Js.Obj(
        "array" -> element,
        "attributes" -> writeJs(attributes)
      )

    case NamedUnionF(alternatives, attributes) =>
      Js.Obj(
        "union" -> alternatives,
        "attributes" -> writeJs(attributes)
      )

    case IndexedUnionF(alternatives, attributes) =>
      Js.Obj(
        "indexed" -> alternatives,
        "attributes" -> writeJs(attributes)
      )

    case ValueF(tpe, attributes) =>
      Js.Obj(
        "type" -> Type.asString(tpe),
        "attributes" -> writeJs(attributes)
      )
  }

  val coalgebra: Coalgebra[SchemaF, Js.Value] = Coalgebra[SchemaF, Js.Value] {
    case Js.Obj(fields) if fields.contains("type") =>
      val attrs = readJs[Map[String, Attribute]](fields("attributes"))
      ValueF(Type.fromString(fields("type").str).get, attrs)

    case Js.Obj(fields) if fields.contains("columns") =>
      val attrs = readJs[Map[String, Attribute]](fields("attributes"))
      val elements = fields("columns").arr.view.map { colJs =>
        val header = colJs.obj.get("header").map(_.str)
        Column[Js.Value](colJs("schema"), header)
      }.toVector
      RowF(elements, attrs)

    case Js.Obj(fields) if fields.contains("array") =>
      val attrs = readJs[Map[String, Attribute]](fields("attributes"))
      ArrayF(fields("array"), attrs)

    case Js.Obj(fields) if fields.contains("fields") =>
      val attrs = readJs[Map[String, Attribute]](fields("attributes"))
      ObjF(SortedMap(fields("fields").obj.toSeq: _*), attrs)

    case Js.Obj(fields) if fields.contains("union") =>
      val attrs = readJs[Map[String, Attribute]](fields("attributes"))
      NamedUnionF(SortedMap(fields("union").obj.toSeq: _*), attrs)

    case Js.Obj(fields) if fields.contains("indexed") =>
      val attrs = readJs[Map[String, Attribute]](fields("attributes"))
      IndexedUnionF(fields("indexed").arr.to[Vector], attrs)

    case fuuu =>
      println("============ FUUUUUUU =========")
      pprint.pprintln(fuuu)
      ???
  }

  implicit val scheamReadWrite: ReadWriter[Schema] = upickle.default
    .readwriter[Js.Value]
    .bimap[Schema](schema => {
      val toJsonFn = scheme.cata(algebra)
      toJsonFn(schema)
    }, js => {
      val fromJsFn = scheme.ana(coalgebra)
      fromJsFn(js)
    })
}
