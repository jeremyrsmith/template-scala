package template.macros

import scala.reflect.macros.whitebox

trait ExprUtils {
  val c: whitebox.Context

  import c.universe._


  object Companions {
    val Literal: Tree = reify(template.Literal).tree
    val Ident: Tree = reify(template.Ident).tree
    val Argument: Tree = reify(template.Argument).tree
    val Select: Tree = reify(template.Select).tree
    val Package: Tree = reify(template.Package).tree
    val Widen: Tree = reify(template.Widen).tree
    val Apply: Tree = reify(template.Apply).tree
    val ApplyToType: Tree = reify(template.ApplyToType).tree
    val TypeApply: Tree = reify(template.TypeApply).tree
    val Match: Tree = reify(template.Match).tree
    val CaseDef: Tree = reify(template.CaseDef).tree
    val Bind: Tree = reify(template.Bind).tree
    val Unapply: Tree = reify(template.Unapply).tree
    val TypeTree: Tree = reify(template.TypeTree).tree
    val This: Tree = reify(template.This).tree
    val If: Tree = reify(template.If).tree
    val Try: Tree = reify(template.Try).tree
    val New: Tree = reify(template.New).tree
    val Function: Tree = reify(template.Function).tree
    val Let: Tree = reify(template.Let).tree
    val Invoke: Tree = reify(template.Invoke).tree
  }

  object ExprType {

    val Literal: Type = weakTypeOf[template.Literal[_, _]].typeConstructor
    val Ident: Type = weakTypeOf[template.Ident[_, _]].typeConstructor
    val Argument: Type = weakTypeOf[template.Argument[_, _, _]].typeConstructor
    val Select: Type = weakTypeOf[template.Select[_, _, _]].typeConstructor
    val Package: Type = weakTypeOf[template.Package[_]].typeConstructor
    val Widen: Type = weakTypeOf[template.Widen[_, _]].typeConstructor
    val Apply: Type = weakTypeOf[template.Apply[_, _, _]].typeConstructor
    val ApplyToType: Type = weakTypeOf[template.ApplyToType[_, _, _]].typeConstructor
    val TypeApply: Type = weakTypeOf[template.TypeApply[_, _, _]].typeConstructor
    val TypeTree: Type = weakTypeOf[template.TypeTree[_]].typeConstructor
    val Match: Type = weakTypeOf[template.Match[_, _, _]].typeConstructor
    val CaseDef: Type = weakTypeOf[template.CaseDef[_, _, _, _]].typeConstructor
    val Unapply: Type = weakTypeOf[template.Unapply[_, _, _, _]].typeConstructor
    val Bind: Type = weakTypeOf[template.Bind[_, _]].typeConstructor
    val This: Type = weakTypeOf[template.This[_]].typeConstructor
    val If: Type = weakTypeOf[template.If[_, _, _, _]].typeConstructor
    val Try: Type = weakTypeOf[template.Try[_, _, _, _]].typeConstructor
    val New: Type = weakTypeOf[template.New[_, _]].typeConstructor
    val Function: Type = weakTypeOf[template.Function[_, _, _]].typeConstructor
    val Let: Type = weakTypeOf[template.Let[_, _, _, _]].typeConstructor
    val Invoke: Type = weakTypeOf[template.Invoke[_, _, _, _, _]].typeConstructor

    object AppliedType {
      def unapply(typ: Type): Option[(Type, List[Type])] = {
        val tc = typ.typeConstructor
        if(tc.takesTypeArgs)
          Some(tc -> typ.typeArgs)
        else
          None
      }
    }

    object ConstName {
      def unapply(typ: Type): Option[TermName] = typ match {
        case ConstantType(Constant(name: String)) => Some(TermName(name).encodedName.toTermName)
        case _ => None
      }
    }


    def unapply(typ: Type): Option[Tree] = typ match {
      case AppliedType(Literal, List(ConstantType(const), _)) => Some(q"$const")
      case AppliedType(Ident, List(ConstName(name), _)) => Some(c.universe.Ident(name))
      case AppliedType(Select, List(AppliedType(Package, List(ConstantType(Constant(pkg: String)))), ConstName(name), _)) =>
        Some(c.universe.Select(c.parse(pkg), name))
      case AppliedType(Select, List(ExprType(from), ConstName(name), _)) => Some(q"$from.$name")
      case AppliedType(Widen, List(ExprType(prev), target)) => Some(q"$prev: $target")
      case AppliedType(If, List(ExprType(cond), ExprType(ifTrue), ExprType(ifFalse), _)) => Some(q"if($cond) $ifTrue else $ifFalse")
      case AppliedType(Match, List(ExprType(selector), ExprTypes(cases), _)) => Some(q"($selector) match { case ..$cases }")
      case AppliedType(CaseDef, List(ExprType(pat), ExprType(guard), ExprType(body), _)) => Some(c.universe.CaseDef(pat, guard, body))
      case AppliedType(Unapply, List(ExprType(obj), ConstName(method), tArgs, ExprTypes(args))) =>
        Some(pq"$obj(..$args)")
      case AppliedType(Bind, List(ConstName(name), ExprType(body))) => Some(c.universe.Bind(name, body))
      case AppliedType(This, List(ConstantType(Constant(name: String)))) =>
        val sym = c.typecheck(c.parse(name), c.TYPEmode)
        Some(c.universe.This(sym.symbol))
      case AppliedType(New, List(ExprTypess(args), cls)) => Some(q"new $cls(...$args)")
      case AppliedType(Let, List(ConstName(name), ExprType(value), ExprType(in), _)) =>
        Some(q"val $name = $value; $in")
      case AppliedType(Invoke, List(ExprType(obj), ConstName(method), tArgs, ExprTypess(args), _)) =>
        val typeArgs = tArgs.typeArgs
        Some {
          if(typeArgs.nonEmpty)
            q"$obj.$method[..$typeArgs](...$args)"
          else
            q"$obj.$method(...$args)"
        }
      case AppliedType(ApplyToType, List(targetType, ExprTypess(args), _)) =>
        Some {
          args.foldLeft[Tree](c.universe.TypeTree(targetType)) {
            (accum, next) => c.universe.Apply(accum, next)
          }
        }
      case AppliedType(TypeTree, List(tt)) => Some(c.universe.TypeTree(tt))
      case other =>
        println(other)
        None
    }
  }


  object ExprTree {
    def nameType(name: Name): Type = c.internal.constantType(Constant(name.decodedName.toString))

    def nameStr(name: Name): Tree = {
      val result = q"${name.decodedName.toString}.asInstanceOf[${nameType(name)}]"
      result
    }

    def tupleTypess(treess: List[List[Tree]]): Type = {
      val tupleTypes = treess.map {
        trees =>
          val treeTypes = trees.map(tree => c.typecheck(tree).tpe)
          c.typecheck(tq"(..$treeTypes)", c.TYPEmode).tpe
      }
      c.typecheck(tq"(..$tupleTypes)", c.TYPEmode).tpe
    }
    def tupless(treess: List[List[Tree]]): Tree = {
      val tuples = treess.map {
        trees => q"(..$trees)"
      }
      q"(..$tuples)"
    }

    def unapply(tree: Tree): Option[Tree] = tree match {
      case l @ Literal(const) => Some(
        q"${Companions.Literal}.apply[${c.internal.constantType(const)}, ${l.tpe.widen}]($l)"
      )
      case i @ Ident(name) => Some(
        q"${Companions.Ident}.apply(${nameStr(name)}).asInstanceOf[${appliedType(ExprType.Ident, nameType(name), i.tpe.widen)}]"
      )
      case s @ Select(Stable(pkgType, pkgStr), NameStr(nameType, nameStr)) =>
        val pkgTree = c.typecheck(q"${Companions.Package}.apply[$pkgType]($pkgStr)")
        Some(
          q"${Companions.Select}.apply[${pkgTree.tpe}, $nameType, ${s.tpe}]($pkgTree, $nameStr)"
        )
      case s @ Select(ExprTree(qualExpr), name) => Some(
        q"${Companions.Select}.apply[${c.typecheck(qualExpr).tpe}, ${nameType(name)}, ${s.tpe}]($qualExpr, ${nameStr(name)})"
      )
      case Typed(ExprTree(expr), tpt) => Some(
        q"${Companions.Widen}.apply[${c.typecheck(expr).tpe}, ${tpt.tpe}]($expr)"
      )
      case i @ If(ExprTree(cond), ExprTree(thenp), ExprTree(elsep)) => Some(
        q"${Companions.If}.apply[${c.typecheck(cond).tpe}, ${c.typecheck(thenp).tpe}, ${c.typecheck(elsep).tpe}, ${i.tpe}]($cond, $thenp, $elsep)"
      )
      case m @ Match(ExprTree(selector), cases) =>
        val caseExprs = cases.map {
          case ExprTree(caseDef) => caseDef
        }
        val caseTypes = caseExprs.map(c.typecheck(_).tpe)

        Some(
          q"${Companions.Match}.apply[${c.typecheck(selector).tpe}, (..$caseTypes), ${m.tpe}]($selector, (..$caseExprs))"
        )
      case cd @ CaseDef(Pattern(pat), EmptyTree, ExprTree(body)) =>
        val Some(literalTrue) = ExprTree.unapply(c.typecheck(q"true"))
        Some(
          q"${Companions.CaseDef}.apply[${c.typecheck(pat).tpe}, ${c.typecheck(literalTrue).tpe}, ${c.typecheck(body).tpe}, ${cd.tpe.widen}]($pat, $literalTrue, $body)"
        )
      case cd @ CaseDef(Pattern(pat), ExprTree(guard), ExprTree(body)) => Some(
        q"${Companions.CaseDef}.apply[${c.typecheck(pat).tpe}, ${c.typecheck(guard).tpe}, ${c.typecheck(body).tpe}, ${cd.tpe.widen}]($pat, $guard, $body)"
      )
      case t @ This(typeName) =>
        val sym = t.symbol.asType
        val name = sym.fullName
        val nameStr = Constant(name)
        val nameType = c.internal.constantType(nameStr)
        Some(
          q"${Companions.This}.apply[$nameType]($nameStr)"
        )
      case Block(stats, ExprTree(expr)) =>
        stats.map {
          case v @ ValDef(_, name, tpt, rhs @ ExprTree(rhsExpr)) =>
            val typ = tpt.tpe match {
              case NoType => rhs.tpe.widen
              case t => t
            }
            Some((in: Tree) => q"${Companions.Let}.apply[${nameType(name)}, ${c.typecheck(rhsExpr).tpe}, ${c.typecheck(in).tpe}, $typ](${nameStr(name)}, $rhsExpr, $in)")
          case other =>
            c.error(other.pos, "Only value definitions are allowed in a block, except for the result expression")
            None
        }.foldRight(Option(identity[Tree] _)) {
          (nextOpt, accumOpt) => accumOpt.flatMap {
            accum => nextOpt.map(next => accum andThen next)
          }
        }.map(letter => letter(expr))
      case t @ InvokeTree(Select(ExprTree(obj), name), TypeTrees(typeArgs), ExprTreess(paramLists)) =>
        val objType = c.typecheck(obj).tpe
        val tType = c.typecheck(t).tpe

        Some(
          q"${Companions.Invoke}.apply[${c.typecheck(obj).tpe}, ${nameType(name)}, (..$typeArgs), ${tupleTypess(paramLists)}, ${c.typecheck(t).tpe}]($obj, ${nameStr(name)}, ${tupless(paramLists)})"
        )
      case t @ InvokeTree(ExprTree(obj), TypeTrees(typeArgs), ExprTreess(paramLists)) =>
        val objType = c.typecheck(obj).tpe
        val tType = c.typecheck(t).tpe
        val NameStr(applyNameType, applyNameStr) = TermName("apply")
        Some(
          q"${Companions.Invoke}.apply[${c.typecheck(obj).tpe}, $applyNameType, (..$typeArgs), ${tupleTypess(paramLists)}, ${c.typecheck(t).tpe}]($obj, $applyNameStr, ${tupless(paramLists)})"
        )
      case other =>
        c.error(other.pos, "Could not convert to template expression")
        None
    }
  }

  object NameStr {
    def unapply(name: Name): Option[(Type, Tree)] = {
      val const = Constant(name.decodedName.toString)
      Some(c.internal.constantType(const) -> Literal(const))
    }
  }

  object Stable {
    def unapply(tree: Tree): Option[(Type, Tree)] = tree.symbol match {
      case sym if sym.isPackage =>
        val const = Constant(sym.fullName)
        Some(c.internal.constantType(const) -> Literal(const))
      case _ => None
    }
  }

  object Pattern {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Bind(NameStr(nameType, nameStr), ExprTree(body)) => Some(
        q"${Companions.Bind}.apply[$nameType, ${c.typecheck(body).tpe}]($nameStr, $body)"
      )
      case UnapplyTree(Select(ExprTree(obj), NameStr(methType, methStr)), TypeTrees(typeArgs), ExprTrees(argExprs)) =>
        val args = c.typecheck(q"(..$argExprs)")
        Some(
          q"${Companions.Unapply}.apply[${c.typecheck(obj).tpe}, $methType, (..$typeArgs), ${args.tpe}]($obj, $methStr, $args)"
        )
      case UnapplyTree(ExprTree(obj), TypeTrees(typeArgs), ExprTrees(argExprs)) =>
        val args = c.typecheck(q"(..$argExprs)")
        val NameStr(methType, methStr) = TermName("unapply")
        Some(
          q"${Companions.Unapply}.apply[${c.typecheck(obj).tpe}, $methType, (..$typeArgs), ${args.tpe}]($obj, $methStr, $args)"
        )
      case InvokeTree(Pattern(obj), TypeTrees(typeArgs), List(Patterns(argExprs))) =>
        val args = c.typecheck(q"(..$argExprs)")
        val NameStr(methType, methStr) = TermName("unapply")
        Some(
          q"${Companions.Unapply}.apply[${c.typecheck(obj).tpe}, $methType, (..$typeArgs), ${args.tpe}]($obj, $methStr, $args)"
        )
      case ExprTree(expr) => Some(expr)
      case _ => None
    }
  }

  object Patterns {
    def unapply(trees: List[Tree]): Option[List[Tree]] = trees.foldRight(Option(List.empty[Tree])) {
      case (Pattern(next), Some(accum)) => Some(next :: accum)
      case _ => None
    }
  }

  object UnapplyTree {
    def unapply(tree: Tree): Option[(Tree, List[Tree], List[Tree])] = tree match {
      case UnApply(InvokeTree(fun, typeArgs, List(List(Ident(TermName("<unapply-selector>"))))), args) =>
        Some((fun, typeArgs, args))

      case _ => None
    }
  }

  object InvokeTree {
    def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] = tree match {
      case Apply(InvokeTree(fn, typeArgs, paramLists), args) => Some((fn, typeArgs, paramLists :+ args))
      case Apply(tt @ TypeTree(), applyArgs) =>
        // used for match cases like `case a :: b :: c`
        val mtpe = tt.tpe
        val msym = mtpe.typeSymbol
        val tpe = tt.tpe.resultType
        tpe.companion.typeSymbol match {
          case NoSymbol => None
          case sym if sym.isModuleClass =>
            val symTree = c.typecheck(q"${sym.asClass.module.asTerm}")
            Some((symTree, tpe.typeArgs.map(TypeTree(_)), List(applyArgs)))
        }
      case Apply(fn, args) => Some((fn, Nil, List(args)))
      case TypeApply(fn, typeArgs) => Some((fn, typeArgs, Nil))

      case _ => None
    }
  }

  object TypeTrees {
    def unapply(trees: List[Tree]): Option[List[Type]] = trees.map {
      case t @ TypeTree() => Some(t.tpe)
      case _ => None
    }.foldRight(Option(List.empty[Type])) {
      (nextOpt, accumOpt) => accumOpt.flatMap(accum => nextOpt.map(next => next :: accum))
    }
  }

  object ExprTrees {
    def unapply(trees: List[Tree]): Option[List[Tree]] = trees.foldRight(Option(List.empty[Tree])) {
      case (ExprTree(expr), Some(exprs)) => Some(expr :: exprs)
      case _ => None
    }
  }

  object ExprTypes {
    def unapply(typ: Type): Option[List[Tree]] = typ match {
      case typ if typ =:= weakTypeOf[Unit] => Some(Nil)
      case tuple if isTupleType(tuple) =>
        tuple.typeArgs.foldRight(Option(List.empty[Tree])) {
          case (ExprType(t), Some(accum)) => Some(t :: accum)
          case _ => None
        }
      case ExprType(single) => Some(List(single))
      case _ => None
    }
  }

  object ExprTreess {
    def unapply(treess: List[List[Tree]]): Option[List[List[Tree]]] = treess.foldRight(Option(List.empty[List[Tree]])) {
      case (ExprTrees(exprs), Some(exprss)) => Some(exprs :: exprss)
      case _ => None
    }
  }

  object ExprTypess {
    def unapply(typ: Type): Option[List[List[Tree]]] = typ match {
      case typ if typ =:= weakTypeOf[Unit] => Some(Nil)
      case tuple if isTupleType(typ) && tuple.typeArgs.forall(isTupleType) =>
        tuple.typeArgs.foldRight(Option(List.empty[List[Tree]])) {
          case (ExprTypes(t), Some(accum)) => Some(t :: accum)
          case _ =>
            println("A")
            None
        }
      case ExprTypes(singleList) => Some(List(singleList))
      case ExprType(single) => Some(List(List(single)))
      case other =>
        println("B", other)
        None
    }
  }

  def isTupleType(typ: Type): Boolean =
    (typ.typeSymbol.owner == symbolOf[Tuple1[_]].owner) &&
      (typ.typeSymbol.name.toString startsWith "Tuple")
}
