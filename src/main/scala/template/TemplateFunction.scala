package template

trait TemplateFunction[In, Out] {
  type Arguments
  type BodyExpr <: Expr[Out]
}

object TemplateFunction {

  type Aux[In, Out, Body <: Expr[Out], Args] = TemplateFunction[In, Out] { type BodyExpr = Body; type Arguments = Args }

  def apply[R, Body <: Expr[R]](fn: () => R): TemplateFunction0[R, Body] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, R, Body <: Expr[R], Args](fn: A => R): TemplateFunction1[A, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, R, Body <: Expr[R], Args](fn: (A, B) => R): TemplateFunction2[A, B, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, R, Body <: Expr[R], Args](fn: (A, B, C) => R): TemplateFunction3[A, B, C, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, D, R, Body <: Expr[R], Args](fn: (A, B, C, D) => R): TemplateFunction4[A, B, C, D, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, D, E, R, Body <: Expr[R], Args](fn: (A, B, C, D, E) => R): TemplateFunction5[A, B, C, D, E, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, D, E, F, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F) => R): TemplateFunction6[A, B, C, D, E, F, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, D, E, F, G, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G) => R): TemplateFunction7[A, B, C, D, E, F, G, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, D, E, F, G, H, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G, H) => R): TemplateFunction8[A, B, C, D, E, F, G, H, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, D, E, F, G, H, I, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G, H, I) => R): TemplateFunction9[A, B, C, D, E, F, G, H, I, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]
  def apply[A, B, C, D, E, F, G, H, I, J, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G, H, I, J) => R): TemplateFunction10[A, B, C, D, E, F, G, H, I, J, R, Body, Args] {} = macro macros.TemplateFunction.create[R, Body]

  // implicit conversions from function to template function; for DSL use cases
  implicit def convert[R, Body <: Expr[R]](fn: () => R): Aux[(), R, Body, ()] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, R, Body <: Expr[R], Args](fn: A => R): Aux[A, R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, R, Body <: Expr[R], Args](fn: (A, B) => R): Aux[(A, B), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, R, Body <: Expr[R], Args](fn: (A, B, C) => R): Aux[(A, B, C), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, D, R, Body <: Expr[R], Args](fn: (A, B, C, D) => R): Aux[(A, B, C, D), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, D, E, R, Body <: Expr[R], Args](fn: (A, B, C, D, E) => R): Aux[(A, B, C, D, E), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, D, E, F, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F) => R): Aux[(A, B, C, D, E, F), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, D, E, F, G, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G) => R): Aux[(A, B, C, D, E, F, G), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, D, E, F, G, H, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G, H) => R): Aux[(A, B, C, D, E, F, G, H), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, D, E, F, G, H, I, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G, H, I) => R): Aux[(A, B, C, D, E, F, G, H, I), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]
  implicit def convert[A, B, C, D, E, F, G, H, I, J, R, Body <: Expr[R], Args](fn: (A, B, C, D, E, F, G, H, I, J) => R): Aux[(A, B, C, D, E, F, G, H, I, J), R, Body, Args] = macro macros.TemplateFunction.create[R, Body]

}

trait TemplateFunction0[R, Body <: Expr[R]] extends TemplateFunction[Unit, R] with Function0[R] {
  type BodyExpr = Body
  type Arguments = ()
  def inline(): R = macro macros.TemplateFunction.inline0[R, Body]
  def andThen[NextR, NextBody <: Expr[NextR], Args, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, Args]
  ): TemplateFunction0[NextR, ResultBody] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Unit, Args]
}

trait TemplateFunction1[A, R, Body <: Expr[R], Args] extends TemplateFunction[A, R] with (A => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A): R = macro macros.TemplateFunction.inline1[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction1[A, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction2[A, B, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B), R] with ((A, B) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B): R = macro macros.TemplateFunction.inline2[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction2[A, B, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction3[A, B, C, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, C), R] with ((A, B, C) => R) {
  type BodyExpr = Body
  def inline(a: A, b: B, c: C): R = macro macros.TemplateFunction.inline3[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction3[A, B, C, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction4[A, B, C, D, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, C, D), R] with ((A, B, C, D) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B, c: C, d: D): R = macro macros.TemplateFunction.inline4[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction4[A, B, C, D, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction5[A, B, C, D, E, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, C, D, E), R] with ((A, B, C, D, E) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B, c: C, d: D, e: E): R = macro macros.TemplateFunction.inline5[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction5[A, B, C, D, E, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction6[A, B, C, D, E, F, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, C, D, E, F), R] with ((A, B, C, D, E, F) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B, c: C, d: D, e: E, f: F): R = macro macros.TemplateFunction.inline6[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction6[A, B, C, D, E, F, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction7[A, B, C, D, E, F, G, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, C, D, E, F, G), R] with ((A, B, C, D, E, F, G) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B, c: C, d: D, e: E, f: F, g: G): R = macro macros.TemplateFunction.inline7[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction7[A, B, C, D, E, F, G, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction8[A, B, C, D, E, F, G, H, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, D, E, F, G, H), R] with ((A, B, C, D, E, F, G, H) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H): R = macro macros.TemplateFunction.inline8[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction8[A, B, C, D, E, F, G, H, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction9[A, B, C, D, E, F, G, H, I, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, C, D, E, F, G, H, I), R] with ((A, B, C, D, E, F, G, H, I) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I): R = macro macros.TemplateFunction.inline9[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction9[A, B, C, D, E, F, G, H, I, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}

trait TemplateFunction10[A, B, C, D, E, F, G, H, I, J, R, Body <: Expr[R], Args] extends TemplateFunction[(A, B, C, D, E, F, G, H, I, J), R] with ((A, B, C, D, E, F, G, H, I, J) => R) {
  type BodyExpr = Body
  type Arguments = Args
  def inline(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J): R = macro macros.TemplateFunction.inline10[R, Body, Args]
  def andThen[NextR, NextBody <: Expr[NextR], NextArgs, ResultBody <: Expr[NextR]](
    next: TemplateFunction1[R, NextR, NextBody, NextArgs]
  ): TemplateFunction10[A, B, C, D, E, F, G, H, I, J, NextR, ResultBody, Args] {} = macro macros.TemplateFunction.andThen[R, NextR, NextBody, ResultBody, Args, NextArgs]
}
