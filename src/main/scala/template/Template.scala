package template

import scala.annotation.StaticAnnotation

class Template[A] {
  def map[B](fn: A => B): Template[B] = ???
    //macro TemplateMacros.mapTemplate[A, B]
  def flatMap[B](fn: A => Template[B]): Template[B] = ???
    //macro TemplateMacros.flatMapTemplate[A, B]

  def inline: A = Inline.asInstanceOf[A]
  def reify: A = macro TemplateMacros.reifyTemplate[A]
}

class TemplateList[A] {
  def map[B](fn: A => B): TemplateList[B] = ???
    //macro TemplateMacros.mapTemplates[A, B]
  def flatMap[B](fn: A => Template[B]): TemplateList[B] = ???
    //macro TemplateMacros.flatMapTemplates[A, B]

  def inline: List[A] = Inline.asInstanceOf[List[A]]

  def foldLeft[B](initial: B)(fn: (B, A) => B): Template[B] = ???
    //macro TemplateMacros.captureFoldLeft[this.type, A, B]
}

trait ProductTemplate[A] {
  def typeName: String = ???
  def fields: ProductFields[A] = ???
}

object ProductTemplate {
  def apply[T](implicit inst: ProductTemplate[T]): inst.type = inst

  implicit def materialize[P <: Product]: Template[ProductTemplate[P]] = ??? //macro TemplateMacros.materializeProductTemplate[P]
}

case class TypeName[A](prev: A) extends Template[String]

trait ProductFields[A] extends TemplateList[ProductField[A]]

case class MapTemplate[A, Prev <: Template[A], B](prev: Prev, fn: A => B) extends Template[B]
case class FlatMapTemplate[A, Prev <: Template[A], B](prev: Prev, fn: A => Template[B]) extends Template[B]

case class MapTemplates[A, Prev <: TemplateList[A], B](prev: Prev, fn: A => B) extends TemplateList[B]
case class FlatMapTemplates[A, Prev <: TemplateList[A], B](prev: Prev, fn: A => Template[B]) extends TemplateList[B]

case class ProductField[A](prev: A) {
  type Type
  def name: String = ???
  def typeName: String = ???
  def typeclassInstance[TC[_]]: TypeclassInstance[Type, TC] = ???
  def value: Type = ???
}

case class FieldName[A](prev: A) extends Template[String]
case class FieldValue[A, Prev](prev: Prev) extends Template[A]

trait TypeclassInstance[A, TC[_]] extends Template[TC[A]]

case class FoldLeftTemplates[A, Prev <: TemplateList[A], B](prev: Prev, initial: B, fn: (B, A) => B) extends Template[B]

object Inline
