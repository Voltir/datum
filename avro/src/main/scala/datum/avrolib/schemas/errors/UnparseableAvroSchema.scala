package datum.avrolib.schemas.errors

case class UnparseableAvroSchema(msg: String) extends Exception(msg)
