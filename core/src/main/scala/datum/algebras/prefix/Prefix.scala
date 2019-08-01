package datum.algebras.prefix
import cats.data.Chain
import datum.algebras.prefix.PathPart._
import datum.patterns.schemas._
import higherkindness.droste.data.{AttrF, Fix}
import higherkindness.droste.data.prelude._
import higherkindness.droste.{Coalgebra, scheme}

import scala.collection.immutable.SortedMap

object Prefix {

  val coalgebra: Coalgebra[SchemaWithPrefixF, (Prefix, Schema)] = Coalgebra[SchemaWithPrefixF, (Prefix, Schema)] {
    case (prefix, schema) =>
      Fix.un[SchemaF](schema) match {
        case v @ ValueF(_, _) =>
          AttrF.apply[SchemaF, Prefix, (Prefix, Schema)](ask = prefix, lower = v)

        case ObjF(fields, props) =>
          val next = ObjF(prefixMap(fields)(k => prefix.append(Field(k))), props)
          AttrF(ask = prefix, lower = next)

        case RowF(columns, props) =>
          val next = columns.zipWithIndex.map {
            case (col, idx) =>
              val prefixed = (prefix.append(Index(idx, col.header)), col.value)
              Column(prefixed, col.header)
          }
          AttrF(prefix, RowF(next, props))

        case ArrayF(conforms, props) =>
          val next = (prefix.append(ArrayPart), conforms)
          AttrF.apply(prefix, ArrayF(next, props))

        case UnionF(alts, props) =>
          val next = prefixMap(alts) { k =>
            prefix.append(NamedSelection(k))
          }
          AttrF(prefix, UnionF(next, props))
      }
  }

  val root: Prefix = Chain.one(Root)

  def apply(schema: Schema): SchemaWithPrefix = {
    val fn = scheme.ana(coalgebra)
    val result = fn((root, schema))
    result
  }

  def toString(prefix: Prefix): String = {
    prefix.iterator.map(PathPart.toString).mkString("/")
  }

  private def prefixMap(map: SortedMap[String, Schema])(
    fn: String => Prefix,
  ): SortedMap[String, (Prefix, Schema)] = {
    val builder = SortedMap.newBuilder[String, (Prefix, Schema)]
    map.foreach {
      case (k, v) =>
        builder += k -> (fn(k), v)
    }
    builder.result()
  }
}
