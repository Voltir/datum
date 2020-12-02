package datum.avrolib

import cats.data.State
import org.apache.avro.{Schema => AvroSchema}

package object schemas {
  type Registry[A] = State[Map[Int, AvroSchema], A]

  // Used to encode original (datum) schema type
  val RECORD_TYPE_KEY = "datum.record.type"

  // Used to encode the original name of a given field or header
  val ORIGINAL_NAME_KEY = "datum.original.name"

  // Used to encode if a header was never set
  val NO_HEADER = "datum.column.noheader"

  // Logical type set and defined in avro based record formats (used for date and timestamps)
  val AVRO_LOGICAL_TYPE = "logicalType"
}
