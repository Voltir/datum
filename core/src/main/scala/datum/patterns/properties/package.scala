package datum.patterns

import higherkindness.droste.{Algebra, Basis, Coalgebra}

package object properties {
  import datum.patterns.properties.PropertyF

  private val algebra: Algebra[PropertyF, Property] = Algebra {
    case BoolPropF(b)               => BoolProp(b)
    case IntPropF(i)                => IntProp(i)
    case StringPropF(s)             => StringProp(s)
    case CollectionPropertyF(props) => CollectionProp(props)
  }

  private val coalgebra: Coalgebra[PropertyF, Property] = Coalgebra {
    case BoolProp(b)           => BoolPropF(b)
    case IntProp(i)            => IntPropF(i)
    case StringProp(s)         => StringPropF(s)
    case CollectionProp(props) => CollectionPropertyF(props)
  }

  implicit val basis: Basis[PropertyF, Property] = Basis.Default(algebra, coalgebra)

  implicit class BoolPropOps(b: Boolean) { def prop: BoolProp = BoolProp(b) }

  implicit class IntPropOps(i: Int) { def prop: IntProp = IntProp(i) }

  implicit class StringPropOps(s: String) { def prop: StringProp = StringProp(s) }
}
