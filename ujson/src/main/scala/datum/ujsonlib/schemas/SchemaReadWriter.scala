package datum.ujsonlib.schemas

import datum.patterns.properties._
import datum.patterns.schemas._
import datum.ujsonlib.properties.PropertyReadWriter
import higherkindness.droste.{Algebra, Coalgebra, scheme}

import scala.collection.immutable.SortedMap

class SchemaReadWriter[API <: upickle.Api](implicit val api: API) {

  import api._
  private implicit val propRW: ReadWriter[Property] = PropertyReadWriter.propertyRW(api)

  val algebra: Algebra[SchemaF, ujson.Value] = Algebra {
    case ObjF(fields, properties) =>
      ujson.Obj(
        "fields" -> fields,
        "properties" -> api.writeJs(properties)
      )

    case RowF(columns, properties) =>
      ujson.Obj(
        "columns" ->
          columns.map(
            col =>
              col.header.map { hdr =>
                ujson.Obj("column" -> col.value, "header" -> hdr)
              } getOrElse {
                ujson.Obj("column" -> col.value)
            }
          ),
        "properties" -> api.writeJs(properties)
      )

    case ArrayF(element, properties) =>
      ujson.Obj(
        "array" -> element,
        "properties" -> api.writeJs(properties)
      )

    case UnionF(alternatives, properties) =>
      ujson.Obj(
        "union" -> alternatives,
        "properties" -> api.writeJs(properties)
      )

    case ValueF(tpe, properties) =>
      ujson.Obj(
        "type" -> Type.asString(tpe),
        "properties" -> api.writeJs(properties)
      )
  }

  val coalgebra: Coalgebra[SchemaF, ujson.Value] = Coalgebra[SchemaF, ujson.Value] {
    case ujson.Obj(fields) if fields.contains("type") =>
      val props = api.read[Map[String, Property]](fields("properties"))
      ValueF(Type.fromString(fields("type").str).get, props)

    case ujson.Obj(fields) if fields.contains("columns") =>
      val props = api.read[Map[String, Property]](fields("properties"))
      val elements = fields("columns").arr.view.map { colJs =>
        val header = colJs.obj.get("header").map(_.str)
        Column[ujson.Value](colJs("column"), header)
      }.toVector
      RowF(elements, props)

    case ujson.Obj(fields) if fields.contains("array") =>
      val props = api.read[Map[String, Property]](fields("properties"))
      ArrayF(fields("array"), props)

    case ujson.Obj(fields) if fields.contains("fields") =>
      val props = api.read[Map[String, Property]](fields("properties"))
      ObjF(SortedMap(fields("fields").obj.toSeq: _*), props)

    case ujson.Obj(fields) if fields.contains("union") =>
      val props = api.read[Map[String, Property]](fields("properties"))
      UnionF(SortedMap(fields("union").obj.toSeq: _*), props)

    case invalid => throw SchemaReadWriter.InvalidSchemaJson(invalid)
  }
}

object SchemaReadWriter {

  def apply[API <: upickle.Api](implicit api: API) = {
    val schemaReadWriter = new SchemaReadWriter[API]
    val schemaRW: api.ReadWriter[Schema] = api
      .readwriter[ujson.Value](api.ReadWriter.join(api.JsValueR, api.JsValueW))
      .bimap[Schema](schema => {
        val toJsonFn = scheme.cata(schemaReadWriter.algebra)
        toJsonFn(schema)
      }, js => {
        val fromJsFn = scheme.ana(schemaReadWriter.coalgebra)
        fromJsFn(js)
      })
    schemaRW
  }

  case class InvalidSchemaJson(invalid: ujson.Value) extends Exception(s"Could not convert json to schema: $invalid")
}
