package datum.ujsonlib.schemas

import datum.patterns.properties._
import datum.patterns.schemas._
import datum.ujsonlib.properties.PropertyReadWriter
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import upickle.default._

import scala.collection.immutable.SortedMap

trait SchemaReadWriter { self: PropertyReadWriter =>

  val algebra: Algebra[SchemaF, ujson.Value] = Algebra {
    case ObjF(fields, properties) =>
      ujson.Obj(
        "fields" -> fields,
        "properties" -> writeJs(properties)
      )

    case RowF(columns, properties) =>
      ujson.Obj(
        "columns" ->
          columns.map(
            col =>
              col.header.map { hdr =>
                ujson.Obj("schema" -> col.value, "header" -> hdr)
              } getOrElse {
                ujson.Obj("schema" -> col.value)
            }
          ),
        "properties" -> writeJs(properties)
      )

    case ArrayF(element, properties) =>
      ujson.Obj(
        "array" -> element,
        "properties" -> writeJs(properties)
      )

    case NamedUnionF(alternatives, properties) =>
      ujson.Obj(
        "union" -> alternatives,
        "properties" -> writeJs(properties)
      )

    case IndexedUnionF(alternatives, properties) =>
      ujson.Obj(
        "indexed" -> alternatives,
        "properties" -> writeJs(properties)
      )

    case ValueF(tpe, properties) =>
      ujson.Obj(
        "type" -> Type.asString(tpe),
        "properties" -> writeJs(properties)
      )
  }

  val coalgebra: Coalgebra[SchemaF, ujson.Value] = Coalgebra[SchemaF, ujson.Value] {
    case ujson.Obj(fields) if fields.contains("type") =>
      val props = read[Map[String, Property]](fields("properties"))
      ValueF(Type.fromString(fields("type").str).get, props)

    case ujson.Obj(fields) if fields.contains("columns") =>
      val props = read[Map[String, Property]](fields("properties"))
      val elements = fields("columns").arr.view.map { colJs =>
        val header = colJs.obj.get("header").map(_.str)
        Column[ujson.Value](colJs("schema"), header)
      }.toVector
      RowF(elements, props)

    case ujson.Obj(fields) if fields.contains("array") =>
      val props = read[Map[String, Property]](fields("properties"))
      ArrayF(fields("array"), props)

    case ujson.Obj(fields) if fields.contains("fields") =>
      val props = read[Map[String, Property]](fields("properties"))
      ObjF(SortedMap(fields("fields").obj.toSeq: _*), props)

    case ujson.Obj(fields) if fields.contains("union") =>
      val props = read[Map[String, Property]](fields("properties"))
      NamedUnionF(SortedMap(fields("union").obj.toSeq: _*), props)

    case ujson.Obj(fields) if fields.contains("indexed") =>
      val props = read[Map[String, Property]](fields("properties"))
      IndexedUnionF(fields("indexed").arr.to[Vector], props)

    case invalid => throw SchemaReadWriter.InvalidSchemaJson(invalid)
  }

  implicit val scheamReadWrite: ReadWriter[Schema] = upickle.default
    .readwriter[ujson.Value]
    .bimap[Schema](schema => {
      val toJsonFn = scheme.cata(algebra)
      toJsonFn(schema)
    }, js => {
      val fromJsFn = scheme.ana(coalgebra)
      fromJsFn(js)
    })
}

object SchemaReadWriter {
  case class InvalidSchemaJson(invalid: ujson.Value) extends Exception(s"Could not convert json to schema: $invalid")
}
