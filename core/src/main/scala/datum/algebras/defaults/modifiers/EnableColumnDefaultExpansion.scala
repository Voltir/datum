package datum.algebras.defaults.modifiers

import datum.patterns.properties._
import datum.patterns.data
import datum.patterns.data.{Data, DataF, EmptyValue, RowValue}
import datum.patterns.schemas.{Column, RowF, SchemaF}

import higherkindness.droste.Algebra
import higherkindness.droste.data.{AttrF, Fix}

import scala.collection.mutable

/* Defaults Algebra Extension
 *
 * When used, will allow for the incoming data rows to have fewer columns then
 * what is specified in the Schema, inserting default values, when available,
 * and empty values otherwise.
 */

object EnableColumnDefaultExpansion {

  val key: String = "enable-default-expansion"

  def enable: (String, Property) = key -> true.prop

  private def columnExpansionFn(
    dataValues: Vector[Data],
    columnFns: Vector[Column[Data => Data]]
  ): Data = {
    val results = mutable.Buffer.empty[Data]

    (dataValues zip columnFns).foreach {
      case (elem, schemaColumn) if elem == data.empty => results += schemaColumn.value(data.empty)
      case (elem, _)                                  => results += elem
    }

    (0 until columnFns.length - dataValues.length).foreach { idx =>
      val offset = idx + dataValues.length
      val fn = columnFns(offset).value
      results += fn(data.empty)
    }
    data.row(results.toVector)
  }

  val algebra: Algebra[AttrF[SchemaF, Data, ?], Data => Data] =
    Algebra[AttrF[SchemaF, Data, ?], Data => Data] {
      case AttrF(_, RowF(elements, _)) =>
        inp =>
          Fix.un[DataF](inp) match {
            case RowValue(values) => columnExpansionFn(values, elements)
            case EmptyValue       => columnExpansionFn(Vector.empty, elements)
            case _                => inp
          }
      case _ => identity
    }
}
