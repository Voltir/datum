package datum.algebras.defaults

import datum.algebras.prefix.Prefix

case class InvalidDefaultDefinition(msg: String) extends Exception(msg)

case class ErrorFoundOnCompile(prefix: Prefix, underlying: Throwable) extends Exception {

  val at: String = Prefix.toString(prefix)

  def asString: String = s"Invalid schema definition at $at:\n\t${underlying.getMessage}"

  override def getMessage: String = asString
}