package datum.avrolib.util

import datum.avrolib.schemas.AVRO_LOGICAL_TYPE
import datum.patterns.properties.TextProp
import datum.patterns.schemas._

object TestSchemas {

  val types = obj()(
    "int" -> value(IntType),
    "long" -> value(LongType),
    "float" -> value(FloatType),
    "double" -> value(DoubleType),
    "text" -> value(TextType),
    "bytes" -> value(BytesType),
    "bool" -> value(BooleanType),
    "date" -> value(DateType),
    "timestamp" -> value(TimestampType, AVRO_LOGICAL_TYPE -> TextProp("timestamp-micros")),
    "timestamp" -> value(TimestampType),
    "date_time" -> value(DateTimeType),
    "zoned_date_time" -> value(ZonedDateTimeType)
  )
}
