package template

import scala.annotation.StaticAnnotation

class templateBody(body: Any) extends StaticAnnotation
class templateImpl[A, B](fn: A => B) extends StaticAnnotation
class foldImpl[A, B](initial: B, fn: (B, A) => B) extends StaticAnnotation
class templateInst[A](body: A) extends StaticAnnotation

//class template extends StaticAnnotation {
//  def macroTransform(annottees: Any*): Any = macro TemplateMacros.annotateTemplate
//}