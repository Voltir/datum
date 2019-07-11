//package datum.ujsonlib.schemas
//import datum.patterns.properties._
//import higherkindness.droste.{Algebra, Coalgebra, scheme}
//import upickle.default.ReadWriter
//
//import scala.collection.immutable.SortedMap
//
//trait PropertyReadWriter {
//
//  val algebra: Algebra[PropertyF, ujson.Value] = Algebra {
//    case BoolPropF(value)                => ujson.Bool(value)
//    case NumPropF(value)                 => ujson.Obj("$type" -> "int", "$prop" -> ujson.Num(value))
//    case CollectionPropF(properties) => ujson.Obj("$type" -> "collection", "$prop" -> ujson.Obj.from(properties))
//  }
//
//  val coalagebra: Coalgebra[PropertyF, ujson.Value] = Coalgebra[PropertyF, ujson.Value] {
//    case ujson.Bool(v) => BoolPropF(v)
//    case ujson.Obj(obj) if obj.contains("$prop") =>
//      obj("$type").str match {
//
//        case "int" => NumPropF(obj("$prop").num.toInt)
//
//        case "collection" =>
//          val builder = SortedMap.newBuilder[String, ujson.Value]
//          obj("$prop").obj.foreach(builder.+=)
//          CollectionPropF.apply[ujson.Value](builder.result())
//      }
//  }
//
//  implicit val attrReadWrite: ReadWriter[Property] = upickle.default
//    .readwriter[ujson.Value]
//    .bimap[Property](
//      prop => {
//        val fn = scheme.cata(algebra)
//        fn(prop)
//      },
//      js => {
//        val fn = scheme.ana(coalagebra)
//        fn(js)
//      }
//    )
//}
