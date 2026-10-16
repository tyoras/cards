package io.tyoras.cards.shared.endpoint.users

import cats.syntax.all.*
import io.circe.derivation.ConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.tyoras.cards.domain.user.model.User as DomainUser
import io.tyoras.cards.domain.user.model.User.ID as UserID
import io.tyoras.cards.util.validation.*
import io.tyoras.cards.util.validation.iron.given
import io.tyoras.cards.util.validation.syntax.*
import io.tyoras.cards.util.codecs.json.given
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.cats.*
import io.scalaland.chimney.partial.syntax.*
import io.github.iltotore.iron.cats.*
import io.tyoras.cards.domain.auth.model.{Password, PasswordChangeRequest}
import io.tyoras.cards.domain.game.{GameTyp, GameType}
import io.tyoras.cards.domain.game.stats.model.PlayerGameStat
import pl.iterators.kebs.circe.snakified.{flatDecoder, flatEncoder}

import java.time.ZonedDateTime

object Payloads:
  object Request:
    final case class Creation(name: Option[String], password: Option[String], about: Option[String]) derives Codec
    object Creation:
      given userDataValidator: Validator[Creation, DomainUser.Data] = new:
        override def validate(c: Creation)(using pf: Option[ParentField]): ValidationResult[DomainUser.Data] = (
          c.name.nestedMandatory[DomainUser.Name]("name"),
          c.about.nestedMandatory[DomainUser.About]("about")
        ).mapN(DomainUser.Data.apply)

      given newUserValidator: Validator[Creation, (DomainUser.Data, Password)] = new:
        override def validate(c: Creation)(using pf: Option[ParentField]): ValidationResult[(DomainUser.Data, Password)] = (
          c.validate[DomainUser.Data],
          c.password.nestedMandatory[Password]("password")
        ).tupled

    final case class PasswordChange(oldPassword: Option[String], newPassword: Option[String]) derives Codec
    object PasswordChange:
      given passwordChangeValidator(using userId: DomainUser.ID): Validator[PasswordChange, PasswordChangeRequest] = new:
        override def validate(r: PasswordChange)(using pf: Option[ParentField]): ValidationResult[PasswordChangeRequest] = (
          userId.valid,
          r.oldPassword.nestedMandatory[Password]("old_password"),
          r.newPassword.nestedMandatory[Password]("new_password")
        ).mapN(PasswordChangeRequest.apply)

  object Response:

    given Decoder[UserID] = flatDecoder
    given Encoder[UserID] = flatEncoder
    final case class User(id: UserID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, name: String, about: String) derives ConfiguredCodec
    object User:
      given Transformer[DomainUser.Existing, Response.User] =
        Transformer.define[DomainUser.Existing, Response.User].enableMethodAccessors.buildTransformer

      given PartialTransformer[Response.User, DomainUser.Existing] =
        PartialTransformer
          .define[Response.User, DomainUser.Existing]
          .withFieldComputedPartial(
            _.data,
            resp =>
              for
                name  <- DomainUser.Name.validatedNec(resp.name).asResult
                about <- DomainUser.About.validatedNec(resp.about).asResult
              yield DomainUser.Data(name, about)
          )
          .buildTransformer

    final case class UserGameStats(game: GameType, won: Int, draw: Int, lost: Int, updatedAt: ZonedDateTime) derives ConfiguredCodec

    object UserGameStats:
      given Transformer[PlayerGameStat.Existing, Response.UserGameStats] =
        Transformer.define[PlayerGameStat.Existing, Response.UserGameStats].enableMethodAccessors.buildTransformer
