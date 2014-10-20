package chatterbox

abstract class Expression {
  def mkStr: String
  def reduce: Expression = this
}

case class Empty() extends Expression {
  def mkStr = ""
}

case class Union(left: Expression, right: Expression) extends Expression {
  def mkStr = "(" + left.mkStr + " + " + right.mkStr + ")"
  override def reduce = Union(left.reduce, right.reduce)
}

case class Symbol(c: Char) extends Expression {
  def mkStr = s"'$c'"
}

case class Star(ex: Expression) extends Expression {
  def mkStr = ex.mkStr + "*"
  override def reduce = Star(ex.reduce)
}

case class Parens(ex: Expression) extends Expression {
  def mkStr = "(" + ex.mkStr + ")"
  override def reduce = {
    if (ex.reduce.isInstanceOf[Symbol]) ex.reduce else Parens(ex.reduce)
  }
}

case class Concat(left: Expression, right: Expression) extends Expression {
  def mkStr = left.mkStr + " " + right.mkStr
  override def reduce = if (left.reduce == Empty()) {
    right.reduce
  } else if (right.reduce == Empty()) {
    left.reduce
  } else {
    Concat(left.reduce, right.reduce)
  }
}
