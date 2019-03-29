package datum.ujsonlib.attributes

import datum.patterns.attributes._
import qq.droste._
import ujson.Js
import upickle.default._

object AttributeReadWriter {

  val algebra: Algebra[AttributesF, Js.Value] = Algebra {
    case Property(value)     => Js.Str(value)
    case NumProperty(value)  => Js.Num(value)
    case BoolProperty(value) => Js.Bool(value)
    case Label(name, value)  => Js.Obj("attr" -> "label", name -> value)
    case Collection(vs)      => Js.Arr(vs)
    case Flag                => Js.Obj()
  }

  val coalgebra: Coalgebra[AttributesF, Js.Value] = Coalgebra[AttributesF, Js.Value] {
    case Js.Str(value) => Property(value)

    case Js.Num(value) => NumProperty(value)

    case Js.Bool(value) => BoolProperty(value)

    case Js.Arr(value) => Collection(value.toVector)

    case Js.Obj(fields) if fields("attr").str == "label" =>
      val (name, value) = fields.filterKeys(_ != "attr").head
      Label(name, value)

    case Js.Obj(fields) if fields.isEmpty => Flag
  }
}

trait AttributeReadWriter {
  import AttributeReadWriter._

  implicit val attrReadWrite: ReadWriter[Attribute] = upickle.default
    .readwriter[Js.Value]
    .bimap[Attribute](
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
