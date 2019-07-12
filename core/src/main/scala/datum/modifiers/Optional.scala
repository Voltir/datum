package datum.modifiers

import datum.patterns.properties._

object Optional {
  val key: String = "optional"
  val enable: (String, Property) = key -> true.prop
}
