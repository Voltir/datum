package datum.ujsonlib
import datum.patterns.schemas.Schema
import datum.ujsonlib.schemas.SchemaReadWriter

package object implicits {
  import upickle.default._
  implicit val schemaRW: ReadWriter[Schema] = SchemaReadWriter(upickle.default)
}
