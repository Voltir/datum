package datum.algebras.defaults
import java.time.format.DateTimeFormatter

import datum.patterns.attributes._
import datum.patterns.data
import datum.patterns.data.Data
import cats.syntax.either._
import cats.instances.either._
import qq.droste.data.Fix
import qq.droste.AlgebraM

trait CompileBoolean {

  def boolean(attribute: Attribute): Either[String, Data] = Fix.un[AttributesF](attribute) match {
    case BoolProperty(value) => Right(data.boolean(value))
    case _                       => Left("Invalid Property - expected a boolean property")
  }
}

trait CompileInteger {

  def integer(attribute: Attribute): Either[String, Data] = Fix.un[AttributesF](attribute) match {
    case NumProperty(value) => Either.catchNonFatal(data.integer(value.toInt)).leftMap(_.getMessage)
    case _                       => Left("Invalid Property - expected a numeric property")
  }
}

trait CompileText {

  def text(attribute: Attribute): Either[String, Data] = Fix.un[AttributesF](attribute) match {
    case Property(value) => Right(data.text(value))
    case _                    => Left("Invalid Property - expected a text")
  }
}

trait CompiledZonedTime {
  import java.time._

  val algebra: AlgebraM[Either[String, ?], AttributesF, Data] = AlgebraM[Either[String, ?], AttributesF, Data] {
    case Property(value) =>
      Either.catchNonFatal {
        val result = ZonedDateTime.parse(value, DateTimeFormatter.ISO_ZONED_DATE_TIME)
        data.zonedTime(result)
      }.leftMap(_.getMessage)

    case _ => Left("Invalid Property - expected a text property (in ISO Zoned Date Time Format)")
  }

  private val mkZoned = qq.droste.scheme.cataM(algebra)

  def zonedTime(attribute: Attribute): Either[String, Data] = mkZoned(attribute)
}

trait AttributeCompilationRules {
  def boolean(attribute: Attribute): Either[String, Data]
  def integer(attribute: Attribute): Either[String, Data]
  def text(attribute: Attribute): Either[String, Data]
  def zonedTime(attribute: Attribute): Either[String, Data]
}

// Alternative rules for turning Attributes into Data can be supplied by creating another
// Rules object with custom overrides
object DefaultCompilationRules
  extends AttributeCompilationRules
  with CompileBoolean
  with CompileInteger
  with CompileText
  with CompiledZonedTime
