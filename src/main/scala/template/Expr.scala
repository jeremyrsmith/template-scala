package template

sealed trait Expr[A] {
  def inline: A = macro macros.Expr.inlineExpr
}

object Expr {
  def apply[T](expr: => T): Out forSome { type Out <: Expr[T] } = macro macros.Expr.create[T]
}

case class Literal[Const, A](
  value: A
) extends Expr[A]

case class Ident[Name <: String, A](
  name: Name
) extends Expr[A]

case class Argument[ParamList <: Int, ParamIndex <: Int, A](
  paramList: ParamList,
  paramIndex: ParamIndex
) extends Expr[A]

case class Select[From, Name <: String, A](
  from: From,
  name: Name
) extends Expr[A]

case class Package[Name <: String](
  name: Name
)

case class This[Name <: String](name: Name)

case class Widen[Prev, A](
  prev: Prev
) extends Expr[A]

case class Apply[Fn, Args, A](
  fn: Fn,
  args: Args
) extends Expr[A]

case class ApplyToType[Type, Args, A](
  args: Args
) extends Expr[A]

case class TypeApply[Fn, TypeArgs, A](
  fn: Fn
) extends Expr[A]

case class Match[Selector, Cases, A](
  selector: Selector,
  cases: Cases
) extends Expr[A]

case class CaseDef[Pattern, Guard <: Expr[Boolean], Body <: Expr[A], A](
  pattern: Pattern,
  guard: Guard,
  body: Body
)

case class Bind[Name <: String, Body](name: Name, body: Body)

case class Unapply[Obj, Method <: String, TypeArgs, Args](
  obj: Obj,
  method: Method,
  args: Args
)

case class TypeTree[Type]()

case class If[Cond <: Expr[Boolean], IfTrue, IfFalse, A](
  cond: Cond,
  ifTrue: IfTrue,
  ifFalse: IfFalse
) extends Expr[A]

case class Try[Body <: Expr[A], Catches, Finalizer <: Expr[Unit], A](
  body: Body,
  catches: Catches,
  finalizer: Finalizer
) extends Expr[A]

case class New[Args, A](
  args: Args
) extends Expr[A]

case class Function[Args : Expressions, Body <: Expr[A], A](
  args: Args,
  body: Body
) extends Expr[A]

case class Let[Identifier <: String, Value, In <: Expr[A], A](
  identifier: Identifier,
  value: Value,
  in: In
) extends Expr[A]

case class Invoke[Obj, Method <: String, TypeArgs, Args, A](
  obj: Obj,
  method: Method,
  args: Args
) extends Expr[A]

/*************************
** Expression functions **
*************************/

case class ExprFunction[Args, Body <: Expr[A], A](args: Args, body: Body) {
  def apply(args: => Args): A = ???
}


object ExprFunction {

  def apply[A, R](fn: A => R): EF forSome { type EF <: ExprFunction[A, _ <: Expr[R], R]} = ???

}

case class Arg[Name <: String, A](name: Name)

trait Expressions[T] {
  type Out
}

object Expressions {

  type Aux[T, Out0] = Expressions[T] { type Out = Out0 }

  implicit def materialize[T, Out]: Aux[T, Out] = macro macros.Expressions.materialize[T, Out]

}


