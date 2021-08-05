package io.tyoras.cards.util.validation

trait Validator[A, B] {
  def validate(a: A)(implicit pf: Option[ParentField] = None): ValidationResult[B]
}

object Validator {
  def apply[A, B](v: Validator[A, B])(pf: Option[ParentField] = None): Validator[A, B] = {
    implicit val parent: Option[ParentField] = pf
    v
  }
}

case class ParentField(name: String) extends AnyVal
