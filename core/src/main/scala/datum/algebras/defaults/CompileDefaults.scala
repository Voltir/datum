package datum.algebras.defaults

import cats.MonadError
import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._
import cats.syntax.all._
import datum.algebras.prefix.{Prefix, SchemaWithPrefixF}
import higherkindness.droste._
import higherkindness.droste.data.{Attr, AttrF}
import higherkindness.droste.data.prelude._

/*
 * Given a Schema with "default" attributes, this:
 *  1) Validates that the default values correspond to the schema correctly
 *  2) Generates the default data value from the attribute value
 *  3) Returns the annotated SchemaF tree with default values attached
 */
class CompileDefaults[M[_]](rules: PropertyToDefaultRules[M])(implicit M: MonadError[M, Throwable]) {

  private val empty = M.pure(data.empty)

  // This function helps Scala infer types
  private def using(
    prefix: Prefix,
    compiled: M[Data],
    schema: SchemaF[Attr[SchemaF, Data]]
  ): M[Attr[SchemaF, Data]] = {
    compiled
      .map { default =>
        Attr.apply(default, schema)
      }
      .adaptError {
        case err => ErrorFoundOnCompile(prefix, err)
      }
  }

  private val algebra: Algebra[SchemaWithPrefixF, M[Attr[SchemaF, Data]]] = {

    Algebra[SchemaWithPrefixF, M[Attr[SchemaF, Data]]] {
      case AttrF(prefix, v @ ValueF(IntType, attrs)) if attrs.contains(key) =>
        using(prefix, rules.integer(attrs(key)), v)

      case AttrF(prefix, v @ ValueF(TextType, attrs)) if attrs.contains(key) =>
        using(prefix, rules.text(attrs(key)), v)

      case AttrF(prefix, v @ ValueF(BooleanType, attrs)) if attrs.contains(key) =>
        using(prefix, rules.boolean(attrs(key)), v)

      case AttrF(prefix, v @ ValueF(ZonedDateTimeType, attrs)) if attrs.contains(key) =>
        using(prefix, rules.zonedTime(attrs(key)), v)

      // Otherwise
      case AttrF(prefix, other) =>
        cats.Traverse[SchemaF].sequence(other).flatMap { ok =>
          using(prefix, empty, ok)
        }
    }
  }

  // Use hylo to add a prefix to the Schema and apply the defaults algebra
  private val generator = scheme.hylo(algebra, Prefix.coalgebra)

  def compile(schema: Schema): M[Attr[SchemaF, Data]] =
    generator((Prefix.root, schema))
}

object CompileDefaults {

  // def apply(rules: PropertyCompilationRules = DefaultCompilationRules): CompileDefaults = new CompileDefaults(rules)
  def apply[M[_]](rules: PropertyToDefaultRules[M])(implicit M: MonadError[M, Throwable]): CompileDefaults[M] = {
    new CompileDefaults(rules)
  }
}
