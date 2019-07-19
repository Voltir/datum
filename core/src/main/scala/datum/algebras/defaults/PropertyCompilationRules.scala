package datum.algebras.defaults
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import cats.MonadError
import datum.patterns.properties._
import datum.patterns.data
import datum.patterns.data.Data
import cats.instances.either._

trait HasMonadError[M[_]] {
  def M: MonadError[M, Throwable]
}

trait BooleanRule[M[_]] { self: HasMonadError[M] =>

  def boolean(property: Property): M[Data] = property match {
    case BoolProp(value) => M.pure(data.boolean(value))
    case _               => M.raiseError(InvalidDefaultDefinition("Invalid Property - expected a boolean property"))
  }
}

trait IntegerRule[M[_]] { self: HasMonadError[M] =>

  def integer(property: Property): M[Data] = property match {
    case NumProp(value) => M.catchNonFatal(data.integer(value.toInt))
    case _              => M.raiseError(InvalidDefaultDefinition("Invalid Property - expected a num property"))
  }
}

trait TextRule[M[_]] { self: HasMonadError[M] =>

  def text(property: Property): M[Data] = property match {
    case TextProp(value) => M.pure(data.text(value))
    case _               => M.raiseError(InvalidDefaultDefinition("Invalid Property - expected a text property"))
  }
}

trait ZonedTimeRule[M[_]] { self: HasMonadError[M] =>

  def zonedTime(property: Property): M[Data] = property match {
    case TextProp(value) =>
      M.catchNonFatal {
        val result = ZonedDateTime.parse(value, DateTimeFormatter.ISO_ZONED_DATE_TIME)
        data.zonedTime(result)
      }
    case _ => M.raiseError(InvalidDefaultDefinition("Invalid Property - expected a text property"))
  }
}

trait PropertyToDefaultRules[M[_]] {
  def boolean(property: Property): M[Data]
  def integer(property: Property): M[Data]
  def text(property: Property): M[Data]
  def zonedTime(property: Property): M[Data]
}

class DefaultPropertyToDefaultRules[M[_]](implicit val M: MonadError[M, Throwable])
  extends PropertyToDefaultRules[M]
  with BooleanRule[M]
  with IntegerRule[M]
  with TextRule[M]
  with ZonedTimeRule[M]
  with HasMonadError[M]

object DefaultPropertyToDefaultRules {
  val either = new DefaultPropertyToDefaultRules[Either[Throwable, ?]]
}
