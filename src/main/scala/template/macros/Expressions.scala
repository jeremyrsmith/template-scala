package template.macros

import scala.reflect.macros.whitebox

class Expressions(val c: whitebox.Context) {
  import c.universe._

  private val Expr = weakTypeOf[template.Expr[_]].typeConstructor
  private def Expressions(In: Type) = appliedType(weakTypeOf[template.Expressions[_]].typeConstructor, In)
  private def ExpressionsAux(In: Type, Out: Type) = appliedType(
    weakTypeOf[template.Expressions.Aux[_, _]].typeConstructor,
    In, Out
  )

  def materialize[T : WeakTypeTag, Out : WeakTypeTag]: Tree = weakTypeOf[T].dealias match {
    case inType @ TupleExprTypes(outTypes) =>
      val outType = c.typecheck(tq"(..$outTypes)").tpe
      q"""
        new ${Expressions(inType)} {
          type Out = $outType
        }: ${ExpressionsAux(inType, outType)}
      """
  }

  object TupleExprTypes {
    def unapply(typ: Type): Option[List[Type]] = typ.typeConstructor match {
      case t if t.typeSymbol.owner == symbolOf[Unit].owner && t.typeSymbol.name.toString.startsWith("Tuple") =>
        t.typeArgs.foldLeft(Option(List.empty[Type])) {
          case (Some(typs), ExprType(returnType)) => Some(typs :+ returnType)
          case _ => None
        }
    }
  }

  object ExprType {
    def unapply(typ: Type): Option[Type] = typ.typeConstructor match {
      case tc if tc <:< Expr => Some(Expr.typeArgs.head.typeSymbol.typeSignatureIn(typ))
      case _ => None
    }
  }

}
