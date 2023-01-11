package datum.avrolib.properties
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node._
import datum.patterns.properties._
import higherkindness.droste.{Algebra, Coalgebra}

import scala.collection.immutable.SortedMap
import scala.collection.JavaConverters._

import com.fasterxml.jackson.databind.node.JsonNodeFactory

object PropertiesReadWriter {

  case class InvalidPropertyDefinition(msg: String) extends Exception(msg)

  // writer algebra
  val algebra: Algebra[PropertyF, JsonNode] = Algebra {
    case BoolPropF(v) => BooleanNode.valueOf(v)
    case NumPropF(v)  => DoubleNode.valueOf(v)
    case TextPropF(v) => TextNode.valueOf(v)
    case ListPropF(vs) =>
      val arr = JsonNodeFactory.instance.arrayNode()
      vs.foreach(arr.add)
      arr
    case CollectionPropF(vs) =>
      val obj = JsonNodeFactory.instance.objectNode()
      vs.foreach { case (k, v) => obj.replace(k, v) }
      obj
  }

  // reader coalgebra
  val coalgebra: Coalgebra[PropertyF, JsonNode] = Coalgebra[PropertyF, JsonNode] { node =>
    node.getNodeType match {
      case JsonNodeType.BOOLEAN => BoolPropF(node.asBoolean())
      case JsonNodeType.NUMBER  => NumPropF(node.asDouble())
      case JsonNodeType.STRING  => TextPropF(node.asText())
      case JsonNodeType.ARRAY =>
        ListPropF(node.elements().asScala.toList)
      case JsonNodeType.OBJECT =>
        val builder = SortedMap.newBuilder[String, JsonNode]
        node.fields().asScala.foreach { entry =>
          builder += (entry.getKey -> entry.getValue)
        }
        CollectionPropF(builder.result())
      case otherwise => throw InvalidPropertyDefinition(s"Invalid json type for a property: $otherwise")
    }
  }
}
