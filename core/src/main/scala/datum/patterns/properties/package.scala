package datum.patterns

import higherkindness.droste.{Algebra, Basis, Coalgebra}

package object properties {

  private val algebra: Algebra[PropertyF, Property] = Algebra {
    case BoolPropF(b)           => BoolProp(b)
    case NumPropF(i)            => NumProp(i)
    case TextPropF(s)           => TextProp(s)
    case CollectionPropF(props) => CollectionProp(props)
  }

  private val coalgebra: Coalgebra[PropertyF, Property] = Coalgebra {
    case BoolProp(b)           => BoolPropF(b)
    case NumProp(i)            => NumPropF(i)
    case TextProp(s)           => TextPropF(s)
    case CollectionProp(props) => CollectionPropF(props)
  }

  implicit val basis: Basis[PropertyF, Property] = Basis.Default(algebra, coalgebra)

  implicit class BoolPropOps(b: Boolean) { def prop: BoolProp = BoolProp(b) }

  implicit class IntPropOps(i: Int) { def prop: NumProp = NumProp(i) }

  implicit class StringPropOps(s: String) { def prop: TextProp = TextProp(s) }
}
