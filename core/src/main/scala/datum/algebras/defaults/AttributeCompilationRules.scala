package datum.algebras.defaults
import java.time.format.DateTimeFormatter

import datum.patterns.properties._
import datum.patterns.data
import datum.patterns.data.Data
import cats.syntax.either._
import cats.instances.either._
import higherkindness.droste.AlgebraM

trait CompileBoolean {

  def boolean(property: Property): Either[String, Data] = property match {
    case BoolProp(value) => Right(data.boolean(value))
    case _               => Left("Invalid Property - expected a boolean property")
  }
}

trait CompileInteger {

  def integer(property: Property): Either[String, Data] = property match {
    case NumProp(value) => Either.catchNonFatal(data.integer(value.toInt)).leftMap(_.getMessage)
    case _              => Left("Invalid Property - expected a numeric property")
  }

}

trait CompileText {

  def text(property: Property): Either[String, Data] = property match {
    case TextProp(value) => Right(data.text(value))
    case _                 => Left("Invalid Property - expected a text")
  }

}

trait CompiledZonedTime {
  import java.time._

  val algebra: AlgebraM[Either[String, ?], PropertyF, Data] = AlgebraM[Either[String, ?], PropertyF, Data] {
    case TextPropF(value) =>
      Either
        .catchNonFatal {
          val result = ZonedDateTime.parse(value, DateTimeFormatter.ISO_ZONED_DATE_TIME)
          data.zonedTime(result)
        }
        .leftMap(_.getMessage)

    case _ => Left("Invalid Property - expected a text property (in ISO Zoned Date Time Format)")
  }

  private val mkZoned = higherkindness.droste.scheme.cataM(algebra)

  def zonedTime(property: Property): Either[String, Data] = mkZoned(property)
}

trait PropertyCompilationRules {
  def boolean(property: Property): Either[String, Data]
  def integer(property: Property): Either[String, Data]
  def text(property: Property): Either[String, Data]
  def zonedTime(property: Property): Either[String, Data]
}

// Alternative rules for turning Propertys into Data can be supplied by creating another
// Rules object with custom overrides
object DefaultCompilationRules
  extends PropertyCompilationRules
  with CompileBoolean
  with CompileInteger
  with CompileText
  with CompiledZonedTime
