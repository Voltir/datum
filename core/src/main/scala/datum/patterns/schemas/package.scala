package datum.patterns

import datum.patterns.properties.Property
import higherkindness.droste.data.Fix

import scala.collection.immutable.SortedMap

package object schemas {
  type PropertyMap = Map[String, Property]

  
  type Schema = Fix[SchemaF]

  def obj(properties: PropertyMap = Map.empty)(fields: (String, Schema)*): Schema = {
    Fix(ObjF(SortedMap(fields: _*), properties))
  }

  def obj(properties: (String, Property)*)(fields: (String, Schema)*): Schema = {
    Fix(ObjF(SortedMap(fields: _*), Map(properties: _*)))
  }

  def row(properties: PropertyMap = Map.empty)(elements: Column[Schema]*): Schema = {
    Fix(RowF(Vector(elements: _*), properties))
  }

  def row(properties: (String, Property)*)(elements: Column[Schema]*): Schema = {
    Fix(RowF(Vector(elements: _*), Map(properties: _*)))
  }

  def col(name: String, schema: Schema): Column[Schema] = {
    Column(schema, Some(name))
  }

  def union(properties: PropertyMap = Map.empty)(alternatives: (String, Schema)*): Schema = {
    Fix(UnionF(SortedMap(alternatives: _*), properties))
  }

  def union(properties: (String, Property)*)(alternatives: (String, Schema)*): Schema = {
    Fix(UnionF(SortedMap(alternatives: _*), Map(properties: _*)))
  }

  def array(properties: PropertyMap = Map.empty)(conforms: Schema): Schema = {
    Fix(ArrayF(conforms, properties))
  }

  def array(properties: (String, Property)*)(conforms: Schema): Schema = {
    Fix(ArrayF(conforms, Map(properties: _*)))
  }

  def value(
    tpe: Type,
    properties: PropertyMap = Map.empty
  ): Schema = {
    Fix.apply[SchemaF](ValueF(tpe, properties))
  }

  def value(
    tpe: Type,
    properties: (String, Property)*
  ): Schema = {
    Fix.apply[SchemaF](ValueF(tpe, Map(properties: _*)))
  }

}
