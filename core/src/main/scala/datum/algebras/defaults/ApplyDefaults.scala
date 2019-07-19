package datum.algebras.defaults
import datum.patterns.data
import datum.patterns.data.{Data, DataF, RowValue}
import datum.patterns.schemas._
import higherkindness.droste.data.{AttrF, _}
import higherkindness.droste.data.prelude._
import higherkindness.droste.{Algebra, scheme}

object ApplyDefaults {

  val algebra: Algebra[AttrF[SchemaF, Data, ?], Data => Data] =
    Algebra[AttrF[SchemaF, Data, ?], Data => Data] {
      // handle cases for `ObjF`s that can either themselves be missing, or contain missing fields
      case AttrF(_, ObjF(schemaFields, _)) =>
        inp =>
          Fix.un[DataF](inp) match {

            // Handle case for when the obj is missing, but might have default fields
            case data.EmptyValue =>
              val nonEmptyFields = schemaFields.mapValues(fn => fn(data.empty)).filter {
                case (_, result) if result == data.empty => false
                case _                                   => true
              }
              if (nonEmptyFields.isEmpty) data.empty
              else data.obj(nonEmptyFields)

            // Handle case for possibly missing fields in the object
            case data.ObjValue(dataFields) =>
              val missingFields = schemaFields.filterKeys(k => !dataFields.keySet.contains(k))
              val defaultFields = missingFields.mapValues(fn => fn(data.empty))

              // Return new data.Obj type with possibly new default fields added
              data.obj(dataFields ++ defaultFields)

            // Otherwise return the input value
            case _ => inp
          }

      // handle cases for `RowF`s that can either themselves be missing, or contain missing fields
      case AttrF(_, RowF(elements, _)) =>
        inp =>
          Fix.un[DataF](inp) match {
            case data.RowValue(dataValues) if dataValues.length == elements.length =>
              val updated = (dataValues zip elements) map {
                case (inpColumn, schemaColumn) =>
                  schemaColumn.value(inpColumn)
              }
              Fix[DataF](RowValue(updated))

            case _ => inp
          }

      // Handle case for replacing empty inputs with default value
      case AttrF(default, schema) if schema.properties.contains(key) =>
        inp =>
          Fix.un[DataF](inp) match {
            case data.EmptyValue => default
            case _               => inp
          }

      // Otherwise, just return the input
      case _ => identity
    }

  def using(
    annotated: Attr[SchemaF, Data],
    extensions: Algebra[AttrF[SchemaF, Data, ?], Data => Data]*
  ): Data => Data = {
    val makeFn =
      if (extensions.nonEmpty) {
        val extendedAlgebra = Algebra[AttrF[SchemaF, Data, ?], Data => Data] { schemaWithDefaults =>
          (algebra +: extensions).map(_.run(schemaWithDefaults)).reduceLeft(_ andThen _)
        }
        scheme.cata(extendedAlgebra)
      } else {
        scheme.cata(algebra)
      }
    makeFn(annotated)
  }
}
