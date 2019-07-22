package datum.algebras
import datum.algebras.prefix.Prefix
import datum.patterns.properties.Property

package object defaults {
  val key: String = "default"

  def use(property: Property): (String, Property) = key -> property

  case class InvalidDefaultDefinition(msg: String) extends Exception(msg)

  case class ErrorFoundOnCompile(prefix: Prefix, underlying: Throwable) extends Exception {

    val at: String = Prefix.toString(prefix)

    def asString: String = s"Invalid schema definition at $at:\n\t${underlying.getMessage}"

    override def getMessage: String = asString
  }

}
