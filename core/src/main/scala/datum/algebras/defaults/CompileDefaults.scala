package datum.algebras.defaults

import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._

import cats.syntax.either._
import higherkindness.droste._
import higherkindness.droste.data.Attr
import scala.collection.mutable

/*
 * Given a Schema with "default" attributes, this:
 *  1) Validates that the default values correspond to the schema correctly
 *  2) Generates the default data value from the attribute value
 *  3) Returns the annotated SchemaF tree with default values attached
 */
class CompileDefaults(rules: PropertyCompilationRules) {

  // Mutable class to collect data about errors for a given Schema
  private[this] case class ErrorFound(error: String) {
    private val paths: mutable.Buffer[String] = mutable.Buffer.empty

    def updatePath(t: String): ErrorFound = {
      paths.prepend(t)
      this
    }

    def asString: String = s"Invalid schema definition at { ${paths.mkString("/")} }:\n\t$error"
  }

  private val empty = Right(data.empty)

  // This function helps Scala infer types
  private def using(
    compiled: Either[String, Data],
    schema: SchemaF[Attr[SchemaF, Data]]
  ): Either[ErrorFound, Attr[SchemaF, Data]] = {
    compiled
      .map { default =>
        Attr.apply(default, schema)
      }
      .leftMap(ErrorFound.apply)
  }

  private val algebra: Algebra[SchemaF, Either[ErrorFound, Attr[SchemaF, Data]]] = {
    def withType(tpe: Type)(result: Either[ErrorFound, Attr[SchemaF, Data]]) = {
      result.leftMap(x => x.updatePath(s"(${Type.asString(tpe)})"))
    }

    Algebra[SchemaF, Either[ErrorFound, Attr[SchemaF, Data]]] {
      case v @ ValueF(IntType, attrs) if attrs.contains(key) =>
        withType(IntType) { using(rules.integer(attrs(key)), v) }

      case v @ ValueF(TextType, attrs) if attrs.contains(key) =>
        withType(TextType) { using(rules.text(attrs(key)), v) }

      case v @ ValueF(BooleanType, attrs) if attrs.contains(key) =>
        withType(BooleanType) { using(rules.boolean(attrs(key)), v) }

      case v @ ValueF(_, _) => using(empty, v)

      case ObjF(fields, attrs) =>
        fields.collectFirst {
          case (k, Left(err)) => Left(err.updatePath(k))
        } getOrElse {
          val valid = fields.mapValues {
            case Right(ok) => ok
            case Left(err) => throw new Exception(s"Impossible: $err")
          }
          using(empty, ObjF(valid, attrs))
        }

      case RowF(elements, attrs) =>
        elements.zipWithIndex.collectFirst {
          case (Column(Left(err), header), idx) =>
            val path = s"[${if (header.isDefined) header.get else s"$idx"}]"
            Left(err.updatePath(path))
        } getOrElse {
          using(empty, RowF(elements.map(x => x.copy(x.value.right.get)), attrs))
        }

      case otherwise => Left(ErrorFound(s"TODO map: $otherwise"))

    }
  }

  private val generator = scheme.cata(algebra)

  def compile(schema: Schema): Either[String, Attr[SchemaF, Data]] = {
    val compileFn = generator(schema)
    compileFn.leftMap(_.asString)
  }
}

object CompileDefaults {
  def apply(rules: PropertyCompilationRules = DefaultCompilationRules): CompileDefaults = new CompileDefaults(rules)
}
