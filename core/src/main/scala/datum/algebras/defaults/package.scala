package datum.algebras
import datum.patterns.properties.Property

package object defaults {
  val key: String = "default"

  def use(property: Property): (String, Property) = key -> property

  case class InvalidDefaultDefinition(msg: String) extends Exception(msg)
}
