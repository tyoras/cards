package io.tyoras.cards.util.validation.error

trait ErrorField {
  def code: String

  def field: String

  def message: Option[String]
}

case class BasicErrorField(
  override val code: String,
  override val field: String,
  override val message: Option[String]
) extends ErrorField
