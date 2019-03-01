package datum.patterns

import datum.patterns.attributes.{Attribute, AttributeMap}
import qq.droste.data.Fix

import scala.collection.immutable.SortedMap

package object schemas {
  type Schema = Fix[SchemaF]

  def obj(attributes: AttributeMap= Map.empty)(fields: (String, Schema)*): Schema = {
    Fix(ObjF(SortedMap(fields: _*), attributes))
  }

  def row(attributes: AttributeMap = Map.empty)(elements: Column[Schema]*): Schema = {
    Fix(RowF(Vector(elements: _*), attributes))
  }

  def union(attributes: AttributeMap = Map.empty)(alternatives: Schema*): Schema = {
    Fix(UnionF(List(alternatives: _*), attributes))
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
