package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions mapValues (v => Signal(eval(v(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => if (references.contains(name)) eval(getReferenceExpr(name, references), references - name) else Double.NaN
    case Plus(left, right) => eval(left, references) + eval(right, references)
    case Minus(left, right) => eval(left, references) - eval(right, references)
    case Times(left, right) => eval(left, references) * eval(right, references)
    case Divide(left, right) => eval(left, references) / eval(right, references)
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
