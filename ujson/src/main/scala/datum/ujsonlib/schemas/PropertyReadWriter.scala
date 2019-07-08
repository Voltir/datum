package datum.ujsonlib.schemas
import datum.patterns.properties._
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import upickle.default.ReadWriter

import scala.collection.immutable.SortedMap

trait PropertyReadWriter {

  val algebra: Algebra[PropertyF, ujson.Value] = Algebra {
    case BoolPropF(value)                => ujson.Bool(value)
    case IntPropF(value)                 => ujson.Obj("$type" -> "int", "$prop" -> ujson.Num(value))
    case CollectionPropertyF(properties) => ujson.Obj("$type" -> "collection", "$prop" -> ujson.Obj.from(properties))
  }

  val coalagebra: Coalgebra[PropertyF, ujson.Value] = Coalgebra[PropertyF, ujson.Value] {
    case ujson.Bool(v) => BoolPropF(v)
    case ujson.Obj(obj) if obj.contains("$prop") =>
      obj("$type").str match {

        case "int" => IntPropF(obj("$prop").num.toInt)

        case "collection" =>
          val builder = SortedMap.newBuilder[String, ujson.Value]
          obj("$prop").obj.foreach(builder.+=)
          CollectionPropertyF.apply[ujson.Value](builder.result())
      }
  }

  implicit val attrReadWrite: ReadWriter[Property] = upickle.default
    .readwriter[ujson.Value]
    .bimap[Property](
      prop => {
        val fn = scheme.cata(algebra)
        fn(prop)
      },
      js => {
        val fn = scheme.ana(coalagebra)
        fn(js)
      }
    )
}
