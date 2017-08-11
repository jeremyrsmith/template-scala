package template

trait TemplateCompanion[TC[_]] {
  implicit def fromProductTemplate[P <: Product](implicit tplInst: Template[TC[P]]): TC[P] =
    macro TemplateMacros.fromProductTemplate[P]
}
