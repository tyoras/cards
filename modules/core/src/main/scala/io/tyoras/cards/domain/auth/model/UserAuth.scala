package io.tyoras.cards.domain.auth.model

import cats.Show
import io.tyoras.cards.domain.user.model.User

import java.nio.charset.StandardCharsets.UTF_8
import java.time.ZonedDateTime
import cats.implicits.toShow
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.Empty
import io.tyoras.cards.domain.auth.model.UserAuth.{PasswordHashBytes, PasswordSaltBytes}

sealed abstract class UserAuth:
  protected type thisType <: UserAuth

  def userId: User.ID
  def passwordHash: PasswordHashBytes
  def withUpdatedHash(newHash: PasswordHashBytes): thisType
  def passwordSalt: PasswordSaltBytes

object UserAuth:
  type PasswordHashBytes = PasswordHashBytes.T
  object PasswordHashBytes extends RefinedSubtype[Array[Byte], DescribedAs[Not[Empty], "Password hash must be a non-blank array of bytes."]]

  type PasswordSaltBytes = PasswordSaltBytes.T
  object PasswordSaltBytes extends RefinedSubtype[Array[Byte], DescribedAs[Not[Empty], "Password salt must be a non-blank array of bytes."]]

  final case class Data(userId: User.ID, passwordHash: PasswordHashBytes, passwordSalt: PasswordSaltBytes) extends UserAuth:
    override protected type thisType = Data

    override def withUpdatedHash(newHash: PasswordHashBytes): thisType = copy(passwordHash = newHash)
  object Data:
    given Show[Data] = d => s"userId = ${d.userId} | passwordHash = ${new String(d.passwordHash, UTF_8)} | passwordSalt = ${new String(d.passwordSalt, UTF_8)}"

  final case class Existing(createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data) extends UserAuth:
    override protected type thisType = Existing

    override def withUpdatedHash(newHash: PasswordHashBytes): thisType = copy(data = data.copy(passwordHash = newHash))

    def passwordHash: PasswordHashBytes = data.passwordHash

    def passwordSalt: PasswordSaltBytes = data.passwordSalt

    def userId: User.ID = data.userId

  object Existing:
    given Show[Existing] = e => s"created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"
