package datum.patterns

import higherkindness.droste.{Algebra, Basis, Coalgebra}

package object properties {
  import datum.patterns.properties.PropertyF

  private val algebra: Algebra[PropertyF, Property] = Algebra {
    case BoolPropF(b)            => BoolProp(b)
    case IntPropF(i)             => IntProp(i)
    case StringPropF(s)          => StringProp(s)
    case CollectionPropertyF(bb) => CollectionProp(bb)
  }

  private val coalgebra: Coalgebra[PropertyF, Property] = Coalgebra {
    case BoolProp(b)        => BoolPropF(b)
    case IntProp(i)         => IntPropF(i)
    case StringProp(s)      => StringPropF(s)
    case CollectionProp(bb) => CollectionPropertyF(bb)
  }

  implicit val basis: Basis[PropertyF, Property] = Basis.Default(algebra, coalgebra)

  implicit class Omg(b: Boolean) { def prop: BoolProp = BoolProp(b) }

  implicit class OmgInt(i: Int) { def prop: IntProp = IntProp(i) }

  implicit class OmgString(s: String) { def prop: StringProp = StringProp(s) }
}
