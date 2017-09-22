package template.macros

import scala.reflect.macros.whitebox

class Expr(val c: whitebox.Context) extends ExprUtils {
  import c.universe._



  def create[T : WeakTypeTag](expr: Tree): Tree = expr match {
    case ExprTree(exprImpl) => exprImpl
    case _ => c.abort(
      expr.pos,
      "Couldn't create template expression"
    )
  }
  
  def inlineExpr: Tree = c.prefix.actualType.widen match {
    case ExprType(tree) =>
      c.typecheck(tree)
    case other => c.abort(c.enclosingPosition, s"Could not reify expression type $other")
  }

}
