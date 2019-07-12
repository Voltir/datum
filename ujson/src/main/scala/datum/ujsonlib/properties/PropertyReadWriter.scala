package datum.ujsonlib.properties

import datum.patterns.properties._
import higherkindness.droste._
import upickle.default._

import scala.collection.immutable.SortedMap

object PropertyReadWriter {

  val algebra: Algebra[PropertyF, ujson.Value] = Algebra {
    case BoolPropF(value)    => ujson.Bool(value)
    case NumPropF(value)     => ujson.Num(value)
    case TextPropF(value)    => ujson.Str(value)
    case CollectionPropF(vs) => ujson.Obj.from(vs)
  }

  val coalgebra: Coalgebra[PropertyF, ujson.Value] = Coalgebra[PropertyF, ujson.Value] {
    case ujson.Str(value) => TextPropF(value)

    case ujson.Num(value) => NumPropF(value)

    case ujson.Bool(value) => BoolPropF(value)

    case ujson.Obj(fields) =>
      val builder = SortedMap.newBuilder[String, ujson.Value]
      builder ++= fields.iterator
      CollectionPropF(builder.result())
  }
}

trait PropertyReadWriter {
  import PropertyReadWriter._

  implicit val attrReadWrite: ReadWriter[Property] = upickle.default
    .readwriter[ujson.Value]
    .bimap[Property](
      attr => {
        val toJsonFn = scheme.cata(algebra)
        toJsonFn(attr)
      },
      js => {
        val fromJsonFn = scheme.ana(coalgebra)
        fromJsonFn(js)
      }
    )
}
