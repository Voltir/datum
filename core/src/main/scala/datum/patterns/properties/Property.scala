package datum.patterns.properties
import scala.collection.immutable.SortedMap

sealed trait Property extends Product with Serializable
case class BoolProp(value: Boolean) extends Property
case class NumProp(value: Double) extends Property
case class TextProp(value: String) extends Property
case class CollectionProp(values: SortedMap[String, Property]) extends Property
