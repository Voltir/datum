package datum.avrolib.data.errors

sealed trait DatumAvroError extends Throwable

case class InvalidRecordOnWrite(msg: String) extends Exception(msg) with DatumAvroError

case class InvalidRecordOnRead(msg: String) extends Exception(msg) with DatumAvroError
