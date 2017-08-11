package template
package annotation

import scala.annotation.StaticAnnotation

class template extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TemplateMacros.annotateTemplate
}
