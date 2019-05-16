package datum.patterns

import datum.patterns.attributes.{Attribute, AttributeMap}
import qq.droste.data.Fix

import scala.collection.immutable.SortedMap

package object schemas {
  type Schema = Fix[SchemaF]

  def obj(attributes: AttributeMap = Map.empty)(fields: (String, Schema)*): Schema = {
    Fix(ObjF(SortedMap(fields: _*), attributes))
  }

  def obj(attributes: (String, Attribute)*)(fields: (String, Schema)*): Schema = {
    Fix(ObjF(SortedMap(fields: _*), Map(attributes: _*)))
  }

  def row(attributes: AttributeMap = Map.empty)(elements: Column[Schema]*): Schema = {
    Fix(RowF(Vector(elements: _*), attributes))
  }

  def row(attributes: (String, Attribute)*)(elements: Column[Schema]*): Schema = {
    Fix(RowF(Vector(elements: _*), Map(attributes: _*)))
  }

  def col(name: String, schema: Schema): Column[Schema] = {
    Column(schema, Some(name))
  }

  def union(attributes: AttributeMap = Map.empty)(alternatives: (String, Schema)*): Schema = {
    Fix(NamedUnionF(SortedMap(alternatives: _*), attributes))
  }

  def union(attributes: (String, Attribute)*)(alternatives: (String, Schema)*): Schema = {
    Fix(NamedUnionF(SortedMap(alternatives: _*), Map(attributes: _*)))
  }

  def indexed(attributes: AttributeMap = Map.empty)(alternatives: Schema*): Schema = {
    Fix(IndexedUnionF(Vector(alternatives: _*), attributes))
  }

  def indexed(attributes: (String, Attribute)*)(alternatives: Schema*): Schema = {
    Fix(IndexedUnionF(Vector(alternatives: _*), Map(attributes: _*)))
  }

  def array(attributes: AttributeMap = Map.empty)(conforms: Schema): Schema = {
    Fix(ArrayF(conforms, attributes))
  }

  def array(attributes: (String, Attribute)*)(conforms: Schema): Schema = {
    Fix(ArrayF(conforms, Map(attributes: _*)))
  }

  def value(
    tpe: Type,
    attributes: AttributeMap = Map.empty
  ): Schema = {
    Fix.apply[SchemaF](ValueF(tpe, attributes))
  }

  def value(
    tpe: Type,
    attributes: (String, Attribute)*
  ): Schema = {
    Fix.apply[SchemaF](ValueF(tpe, Map(attributes: _*)))
  }

}
