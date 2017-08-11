package template

import scala.collection.immutable.ListMap
import scala.reflect.macros.whitebox

class TemplateMacros(val c: whitebox.Context) {

  import c.universe._

  private def FoldLeftTemplates(A: Type, B: Type)(initial: Tree)(fn: Tree): Tree = {
    val typ = appliedType(weakTypeOf[FoldLeftTemplates[_, _, _]].typeConstructor, A, c.prefix.tree.tpe, B)
    c.typecheck(q"new $typ(${c.prefix}, $initial, $fn)")
  }

  private def templateImpl(fn: Tree) = fn match {
    case FunctionType(List(a), b) =>
      val typ = appliedType(
        weakTypeOf[templateImpl[_, _]], a, b
      )
      Annotation(q"new $typ($fn)")
  }

  private def foldImpl(initial: Tree, fn: Tree) = fn match {
    case FunctionType(List(b, a), _) =>
      val typ = appliedType(
        weakTypeOf[foldImpl[_, _]], a, b
      )
      Annotation(q"new $typ($initial, $fn)")
  }

  private def templateInst(body: Tree) = {
    val typ = appliedType(weakTypeOf[templateInst[_]].typeConstructor, body.tpe)
    Annotation(q"new $typ($body)")
  }

  def captureTemplateFn[A : WeakTypeTag, B : WeakTypeTag](constructor: Type)(fn: Tree): Tree = {
    val typ = appliedType(constructor.typeConstructor, weakTypeOf[A], c.prefix.actualType, weakTypeOf[B])
    val out = c.typecheck(q"new $typ(${c.prefix}, $fn)")
    c.internal.setType(
      out,
      c.internal.annotatedType(List(templateImpl(fn)), out.tpe)
    )
  }

  def mapTemplate[A : WeakTypeTag, B : WeakTypeTag](fn: Tree): Tree =
    captureTemplateFn[A, B](weakTypeOf[MapTemplate[_, _, _]])(fn)

  def flatMapTemplate[A : WeakTypeTag, B : WeakTypeTag](fn: Tree): Tree =
    captureTemplateFn[A, B](weakTypeOf[FlatMapTemplate[_, _, _]])(fn)

  def mapTemplates[A : WeakTypeTag, B : WeakTypeTag](fn: Tree): Tree =
    captureTemplateFn[A, B](weakTypeOf[MapTemplates[_, _, _]])(fn)

  def flatMapTemplates[A : WeakTypeTag, B : WeakTypeTag](fn: Tree): Tree =
    captureTemplateFn[A, B](weakTypeOf[FlatMapTemplates[_, _, _]])(fn)

  def captureFoldLeft[Prev : WeakTypeTag, A : WeakTypeTag, B : WeakTypeTag](initial: Tree)(fn: Tree): Tree = {
    val next = FoldLeftTemplates(weakTypeOf[A], weakTypeOf[B])(initial)(fn)
    c.internal.setType(next, c.internal.annotatedType(List(foldImpl(initial, fn)), next.tpe))
  }

  def fromProductTemplate[P <: Product : WeakTypeTag](tplInst: Tree): Tree = {
    q"$tplInst.reify"
  }

  def materializeProductTemplate[P <: Product : WeakTypeTag]: Tree = {
    val P = weakTypeOf[P]
    val typ = appliedType(weakTypeOf[template.Template[_]].typeConstructor, appliedType(weakTypeOf[ProductTemplate[_]].typeConstructor, P))
    q"new $typ"
  }

  def annotateTemplate(annottees: Tree*): Tree = {
    val results = annottees.map {
      case d @ DefDef(mods, name, tParams, paramLists, tpt, rhs) =>
        val fn = q"{$d}"
        val amods = Modifiers(Flag.IMPLICIT | Flag.MACRO).mapAnnotations(q"new _root_.template.templateBody($fn)" :: _)
        DefDef(amods, name, tParams, Nil, tpt, q"_root_.template.TemplateAnnotationMacros.instantiateTemplate")
        //q"$amods def $name[..$tParams]: $tpt = macro _root_.template.TemplateAnnotationMacros.instantiateTemplate"
      case other => other
    }
    val result =
      q"""{
        import scala.language.experimental.macros
        ..$results
        ()
      }"""
    //c.abort(c.enclosingPosition, "not yet")
    result
  }

  def reifyTemplate[A : WeakTypeTag]: Tree = c.prefix.tree match {
    case q"$pre.map[$t]($fn)" =>
      c.abort(c.enclosingPosition, "not yet")
    case q"$pre.flatMap[$t]($fn)" =>
      c.abort(c.enclosingPosition, "not yet")
    case q"$pre.inline" =>
      c.abort(c.enclosingPosition, "not yet")
    case other =>
      c.abort(c.enclosingPosition, "not yet")
  }


  object FunctionType {
    def unapply(tree: Tree): Option[(List[Type], Type)] = tree match {
      case Function(params, body) => Some(params.map(_.tpt.tpe) -> body.tpe)
      case _ => None
    }
  }
}

object TemplateAnnotationMacros {

  def instantiateTemplate(c: whitebox.Context): c.Tree = {
    import c.universe._
    val app = c.macroApplication
    val tParams = app match {
      case TypeApply(_, tp) => tp
      case Apply(TypeApply(_, tp), _) => tp
      case _ => Nil
    }

    val copier = newStrictTreeCopier

    val impl = app.symbol.annotations.find(_.tree.tpe =:= weakTypeOf[templateBody]).getOrElse {
      c.abort(c.enclosingPosition, s"${app.symbol} has no template implementation")
    }.tree match {
      case Apply(_, List(b @ Block(List(d: DefDef), expr))) if d.vparamss.length <= 1 =>
        c.typecheck(copier.Block(b, List(d), expr)) match {
          case Block(List(dd: DefDef), _) =>
            c.internal.substituteTypes(dd, dd.tparams.map(_.symbol), tParams.map(_.tpe)) match {
              case dd: DefDef => dd
            }
          case _ => c.abort(c.enclosingPosition, "DefDef suddenly changed during tree copy and typecheck")
        }
      case other =>
        val o = other
        println(o)
        c.abort(c.enclosingPosition, s"${app.symbol} has malformed template implementation")
    }

    val implicits = impl.vparamss.headOption.getOrElse(Nil).map {
      case ValDef(_, name, tpt, _) => name -> c.inferImplicitValue(tpt.tpe)
    }.toMap

    val subbed = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(n: TermName) if implicits contains n => implicits(n)
        case other => super.transform(other)
      }
    }.transform(impl.rhs)

    try c.typecheck(subbed)
    catch {
      case err: Throwable =>
        val e = err
        println(e)
        throw e
    }
  }
}