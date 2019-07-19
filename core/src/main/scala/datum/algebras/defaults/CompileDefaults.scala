package datum.algebras.defaults

import cats.{Applicative, MonadError}
import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._
import cats.instances.sortedMap._
import cats.instances.string._
import cats.syntax.all._
import higherkindness.droste._
import higherkindness.droste.data.Attr

import scala.collection.immutable.SortedMap
import scala.collection.mutable

/*
 * Given a Schema with "default" attributes, this:
 *  1) Validates that the default values correspond to the schema correctly
 *  2) Generates the default data value from the attribute value
 *  3) Returns the annotated SchemaF tree with default values attached
 */
class CompileDefaults[M[_]](rules: PropertyToDefaultRules[M])(implicit M: MonadError[M, Throwable]) {

  // Mutable class to collect data about errors for a given Schema
  private[this] case class ErrorFound(underlying: Throwable) extends Exception {
    private val paths: mutable.Buffer[String] = mutable.Buffer.empty

    def updatePath(t: String): ErrorFound = {
      paths.prepend(t)
      this
    }

    def asString: String = s"Invalid schema definition at { ${paths.mkString("/")} }:\n\t${underlying.getMessage}"

    override def getMessage: String = asString
  }

  private val empty = M.pure(data.empty)

  // This function helps Scala infer types
  private def using(
    compiled: M[Data],
    schema: SchemaF[Attr[SchemaF, Data]]
  ): M[Attr[SchemaF, Data]] = {
    compiled
      .map { default =>
        Attr.apply(default, schema)
      }
      .adaptError {
        case err => ErrorFound(err)
      }
  }

  private val algebra2: Algebra[SchemaF, M[Attr[SchemaF, Data]]] = {
    def updateErrorWithType(tpe: Type)(result: M[Attr[SchemaF, Data]]) = {
      result.adaptError {
        case ef @ ErrorFound(_) => ef.updatePath(s"(${Type.asString(tpe)})")
      }
    }
    import cats.instances.vector._

    Algebra[SchemaF, M[Attr[SchemaF, Data]]] {
      case v @ ValueF(IntType, attrs) if attrs.contains(key) =>
        updateErrorWithType(IntType) { using(rules.integer(attrs(key)), v) }

      case v @ ValueF(TextType, attrs) if attrs.contains(key) =>
        updateErrorWithType(IntType) { using(rules.text(attrs(key)), v) }

      case v @ ValueF(BooleanType, attrs) if attrs.contains(key) =>
        updateErrorWithType(IntType) { using(rules.boolean(attrs(key)), v) }

      case v @ ValueF(ZonedDateTimeType, attrs) if attrs.contains(key) =>
        updateErrorWithType(IntType) { using(rules.zonedTime(attrs(key)), v) }

      case v @ ValueF(_, _) => using(empty, v)

      case ObjF(fields, attrs) =>
        val adapted = SortedMap.newBuilder[String, M[Attr[SchemaF, Data]]]
        adapted ++= fields.map {
          case (k, v) =>
            k -> v.adaptError {
              case ef @ ErrorFound(_) => ef.updatePath(k)
            }
        }
        val valid = cats.Traverse[SortedMap[String, ?]].sequence(adapted.result())
        valid.flatMap { ok =>
          using(empty, ObjF(ok, attrs))
        }

      case RowF(elements, attrs) =>
        val adapted = elements.zipWithIndex.map {
          case (Column(value, header), idx) =>
            value.adaptError {
              case ef @ ErrorFound(_) =>
                val path = s"[${header.getOrElse(s"$idx")}]"
                ef.updatePath(path)
            }
        }
        cats.Traverse[Vector].sequence(adapted).flatMap { ok =>
          val asColumn = elements.zip(ok).map {
            case (a, b) => a.copy(value = b)
          }
          using(empty, RowF(asColumn, attrs))
        }

//        elements.zipWithIndex.collectFirst {
//          case (Column(Left(err), header), idx) =>
//            val path = s"[${if (header.isDefined) header.get else s"$idx"}]"
//            Left(err.updatePath(path))
//        } getOrElse {
//          using(empty, RowF(elements.map(x => x.copy(x.value.right.get)), attrs))
//        }

      case otherwise => M.raiseError(InvalidDefaultDefinition(s"Invalid Schema: $otherwise"))
    }
  }

//  private val algebra: Algebra[SchemaF, Either[ErrorFound, Attr[SchemaF, Data]]] = {
//    def withType(tpe: Type)(result: Either[ErrorFound, Attr[SchemaF, Data]]) = {
//      result.leftMap(x => x.updatePath(s"(${Type.asString(tpe)})"))
//    }
//
//    Algebra[SchemaF, Either[ErrorFound, Attr[SchemaF, Data]]] {
//      case v @ ValueF(IntType, attrs) if attrs.contains(key) =>
//        withType(IntType) { using(rules.integer(attrs(key)), v) }
//
//      case v @ ValueF(TextType, attrs) if attrs.contains(key) =>
//        withType(TextType) { using(rules.text(attrs(key)), v) }
//
//      case v @ ValueF(BooleanType, attrs) if attrs.contains(key) =>
//        withType(BooleanType) { using(rules.boolean(attrs(key)), v) }
//
//      case v @ ValueF(_, _) => using(empty, v)
//
//      case ObjF(fields, attrs) =>
//        fields.collectFirst {
//          case (k, Left(err)) => Left(err.updatePath(k))
//        } getOrElse {
//          val valid = fields.mapValues {
//            case Right(ok) => ok
//            case Left(err) => throw new Exception(s"Impossible: $err")
//          }
//          using(empty, ObjF(valid, attrs))
//        }
//
//      case RowF(elements, attrs) =>
//        elements.zipWithIndex.collectFirst {
//          case (Column(Left(err), header), idx) =>
//            val path = s"[${if (header.isDefined) header.get else s"$idx"}]"
//            Left(err.updatePath(path))
//        } getOrElse {
//          using(empty, RowF(elements.map(x => x.copy(x.value.right.get)), attrs))
//        }
//
//      case otherwise => Left(ErrorFound(s"TODO map: $otherwise"))
//
//    }
//  }

  private val generator = scheme.cata(algebra2)

  def compile(schema: Schema): M[Attr[SchemaF, Data]] = generator(schema)
}

object CompileDefaults {

  // def apply(rules: PropertyCompilationRules = DefaultCompilationRules): CompileDefaults = new CompileDefaults(rules)
  def apply[M[_]](rules: PropertyToDefaultRules[M])(implicit M: MonadError[M, Throwable]): CompileDefaults[M] = {
    new CompileDefaults(rules)
  }
}
