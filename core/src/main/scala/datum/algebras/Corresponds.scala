package datum.algebras
import datum.modifiers.Optional
import datum.patterns.data
import datum.patterns.data._
import datum.patterns.schemas._
import qq.droste.data.Fix
import qq.droste.{Algebra, scheme}
import qq.droste.syntax.project._

object Corresponds {

  private def matchValue(fn: PartialFunction[DataF[Fix[DataF]], Boolean])(value: Data): Boolean =
    fn.applyOrElse[DataF[Fix[DataF]], Boolean](Fix.un[DataF](value), _ => false)

  val algebra: Algebra[SchemaF, Data => Boolean] = Algebra {

    case ObjF(schemaFields, _) =>
      _.project match {
        case ObjValue(valueFields) =>
          schemaFields.forall {
            case (key, checkFn) if valueFields.contains(key) =>
              checkFn(valueFields(key))

            // ensure child is checked for cases like option or default values
            case (_, checkFn) => checkFn(data.empty)
          }
        case _ => false
      }

    case RowF(schemaColumns, _) =>
      _.project match {
        case RowValue(values) if schemaColumns.length == values.length =>
          schemaColumns.zip(values).forall {
            case (column, data) =>
              val fn = column.value
              fn(data)
          }
        case _ => false
      }

    case NamedUnionF(alternatives, _) =>
      _.project match {
        case NamedUnionValue(selected, v) =>
          alternatives.get(selected).exists(_.apply(v))
        case _ => false
      }

    case IndexedUnionF(alternatives, _) =>
      _.project match {
        case IndexedUnionValue(idx, v) if idx >= 0 && idx < alternatives.length =>
          alternatives(idx)(v)
        case _ => false
      }

    case ArrayF(fn, _) =>
      _.project match {
        case RowValue(values) => values.forall(fn)
        case _                => false
      }

    case ValueF(TextType, _)          => matchValue { case TextValue(_)      => true }
    case ValueF(IntType, _)           => matchValue { case IntValue(_)       => true }
    case ValueF(LongType, _)          => matchValue { case LongValue(_)      => true }
    case ValueF(FloatType, _)         => matchValue { case FloatValue(_)     => true }
    case ValueF(DoubleType, _)        => matchValue { case DoubleValue(_)    => true }
    case ValueF(BooleanType, _)       => matchValue { case BooleanValue(_)   => true }
    case ValueF(DateType, _)          => matchValue { case DateValue(_)      => true }
    case ValueF(TimestampType, _)     => matchValue { case TimestampValue(_) => true }
    case ValueF(DateTimeType, _)      => matchValue { case DateTimeValue(_)  => true }
    case ValueF(ZonedDateTimeType, _) => matchValue { case ZonedTimeValue(_) => true }
    case ValueF(BytesType, _)         => matchValue { case BytesValue(_)     => true }

  }

  def optional(alg: Algebra[SchemaF, Data => Boolean]): Algebra[SchemaF, Data => Boolean] = Algebra {
    case x if x.attributes.contains(Optional.key) => {
      case data.empty => true
      case otherwise  => alg(x)(otherwise)
    }
    case otherwise => alg(otherwise)
  }

  def define(algebra: Algebra[SchemaF, Data => Boolean] = algebra): Schema => Data => Boolean = {
    val fn = scheme.cata(algebra)
    val result: Schema => Data => Boolean = { s =>
      fn(s)
    }
    result
  }
}
