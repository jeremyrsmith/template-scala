package template.macros

import scala.reflect.macros.whitebox

class TemplateFunction(val c: whitebox.Context) extends ExprUtils {
  import c.universe._

  private def paramType(param: ValDef): Type = param.tpe match {
    case NoType => param.tpt.tpe
    case typ => typ
  }

  private def paramNamesType(params: List[ValDef]): Type = {
    val nameTypes = params.map(_.name).map(ExprTree.nameType)
    c.typecheck(tq"(..$nameTypes)", c.TYPEmode).tpe
  }

  def create[R : WeakTypeTag, Body <: template.Expr[R]](fn: Tree): Tree = fn match {
    case Function(params, body) => body match {
      case ExprTree(exprImpl) =>
        val R = weakTypeOf[R]

        val checkedImpl = c.typecheck(exprImpl)
        val exprType = checkedImpl.tpe
        val fnTypeName = TypeName(s"TemplateFunction${params.length}")

        val applyParamNames = params.map(param => TermName(param.name.decodedName.toString))

        val applyParams = params.zip(applyParamNames) map {
          case (ValDef(mods, _, tpt, _), name) => ValDef(mods, name, tpt, EmptyTree)
        }

        val applyIdents = applyParamNames.map {
          name => Ident(name)
        }

        val typeArgs = params.map(paramType) ::: {
          if (params.nonEmpty)
            List(R, exprType, paramNamesType(params))
          else
            List(R, exprType)
        }

        val fnType = c.typecheck(tq"_root_.template.$fnTypeName[..$typeArgs]", c.TYPEmode).tpe


        val result = q"""
          new $fnType {
            def apply(..$applyParams): $R = inline(..$applyIdents)
          }
        """

        result

      case other => c.abort(other.pos, "Could not splice function body into expression")
    }

    case other => c.abort(other.pos, "Argument must be a function literal")
  }

  def andThen[R : WeakTypeTag, NextR : WeakTypeTag, NextBody : WeakTypeTag, ResultBody : WeakTypeTag, Args: WeakTypeTag, NextArgs : WeakTypeTag](next: Tree): Tree = {
    val R = weakTypeOf[R] // return type of first function
    val NextR = weakTypeOf[NextR]
    val preTpe = c.prefix.actualType

    val unrefined = preTpe.widen match {
      case RefinedType(parents, _) => parents.last
    }

    // TODO: what if first function outputs a tuple? Should we let it be wired to a TemplateFunctionN?

    weakTypeOf[NextArgs] match {
      case nameType @ ConstantType(Constant(name: String)) =>
        val currentCtor = unrefined.typeConstructor
        val currentArgs = unrefined.typeArgs
        val apply = preTpe.member(TermName("apply")).asMethod
        val (applyParams, applyIdents) = apply.typeSignatureIn(preTpe) match {
          case mt @ MethodType(params, ret) => params.zip(argNames(weakTypeOf[Args])).map {
            case (param, argNameStr) =>
              val argType = param.typeSignatureIn(mt)
              val argName = TermName(argNameStr)
              ValDef(Modifiers(), argName, TypeTree(argType), EmptyTree) -> Ident(argName)
          }.unzip
        }

        val newArgs = currentCtor.typeParams.zip(currentArgs).map {
          case (param, _) if param.name.toString == "R" => NextR
          case (param, arg) if param.name.toString == "Body" => appliedType(ExprType.Let, nameType, arg, weakTypeOf[NextBody], weakTypeOf[NextR])
          case (_, arg) => arg
        }


        val newTyp = appliedType(currentCtor, newArgs)
        val result = q"""
          new $newTyp {
            def apply(..$applyParams): $NextR = inline(..$applyIdents)
          }
        """

        result
    }
  }

  def inline0[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag](): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) => expr
    case other => c.abort(c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline1[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](a: Tree): Tree =
    weakTypeOf[Body].dealias match {
      case ExprType(expr) => bindArgs(expr, weakTypeOf[Args])(a)
      case other => c.abort(c.enclosingPosition, s"Could not reify expression type $other")
    }

  def inline2[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b)
    case other => c.abort(c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline3[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline4[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree,
    d: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c, d)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline5[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree,
    d: Tree,
    e: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c, d, e)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline6[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree,
    d: Tree,
    e: Tree,
    f: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c, d, e, f)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline7[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree,
    d: Tree,
    e: Tree,
    f: Tree,
    g: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c, d, e, f, g)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline8[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree,
    d: Tree,
    e: Tree,
    f: Tree,
    g: Tree,
    h: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c, d, e, f, g, h)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline9[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree,
    d: Tree,
    e: Tree,
    f: Tree,
    g: Tree,
    h: Tree,
    i: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c, d, e, f, g, h, i)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  def inline10[R : WeakTypeTag, Body <: template.Expr[R] : WeakTypeTag, Args : WeakTypeTag](
    a: Tree,
    b: Tree,
    c: Tree,
    d: Tree,
    e: Tree,
    f: Tree,
    g: Tree,
    h: Tree,
    i: Tree,
    j: Tree
  ): Tree = weakTypeOf[Body].dealias match {
    case ExprType(expr) =>
      val Args = weakTypeOf[Args]
      bindArgs(expr, weakTypeOf[Args])(a, b, c, d, e, f, g, h, i, j)
    case other => this.c.abort(this.c.enclosingPosition, s"Could not reify expression type $other")
  }

  private def argNames(compoundNames: Type): List[String] = compoundNames match {
    case unit if unit =:= weakTypeOf[Unit] => Nil
    case tuple if tuple.typeSymbol.owner == symbolOf[Tuple1[_]].owner && (tuple.typeSymbol.name.toString startsWith "Tuple") =>
      tuple.typeArgs.map {
        case ConstantType(Constant(name: String)) => name
      }
    case ConstantType(Constant(name: String)) => List(name)
  }


  private def bindArgs(tree: Tree, argTypes: Type)(args: Tree*): Tree = {
    val tq"(..$argNameTypes)" = TypeTree(argTypes)

    val names = argNames(argTypes)
    names.zip(args).foldLeft(tree) {
      case (accum, (name, arg)) =>
        val termName = TermName(name)
        q"{ val $termName = $arg; $accum }"
    }
  }

}
