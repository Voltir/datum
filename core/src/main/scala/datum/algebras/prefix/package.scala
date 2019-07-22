package datum.algebras
import cats.data.Chain
import datum.patterns.schemas.SchemaF
import higherkindness.droste.data.{Attr, AttrF}

package object prefix {

  type Prefix = Chain[PathPart]

  type SchemaWithPrefixF[A] = AttrF[SchemaF, Prefix, A]

  type SchemaWithPrefix = Attr[SchemaF, Prefix]
}
