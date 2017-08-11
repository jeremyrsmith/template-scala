package show

import template._
import template.annotation.template

trait Show[T] {
  def show(t: T): String
}

object Show extends TemplateCompanion[Show] {

  @template implicit def product[P <: Product](implicit tplTemplate: Template[ProductTemplate[P]]): Template[Show[P]] = tplTemplate.map { tpl =>
    new Show[P] {
      def show(p: P): String = {
        tpl.fields.flatMap {
          field =>
            field.typeclassInstance[Show].map {
              inst => field.name + "=" + inst.show(field.value)
            }
        }.foldLeft("") {
          (accum, next) => accum + next + ", "
        }.inline
      }
    }
  }
}
