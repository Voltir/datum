package datum.algebras.prefix

sealed trait PathPart extends Product with Serializable

object PathPart {
  case object Root extends PathPart
  case class Field(name: String) extends PathPart
  case class Index(i: Int) extends PathPart
  case object ArrayPart extends PathPart
  case class NamedSelection(selection: String) extends PathPart
  case class IndexedSelection(selection: Int) extends PathPart

  def toString(part: PathPart): String = part match {
    case Root                        => ""
    case Field(name)                 => name
    case Index(i)                    => i.toString
    case ArrayPart                   => "[]"
    case NamedSelection(selection)   => s"{selected:$selection}"
    case IndexedSelection(selection) => s"{index:$selection}"
  }
}
