package io.tyoras.cards.server.endpoints

import cats.effect.Sync
import cats.implicits.catsSyntaxApplicativeId
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{CursorOp, DecodingFailure, Encoder}
import io.tyoras.cards.util.validation.error.ValidationError
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.DecodingFailures
import org.log4s.{getLogger, Logger}

object ErrorHandling:
  val logger: Logger = getLogger

  final case class ApiMessage(code: String, message: String, errors: List[ApiError] = Nil)
  object ApiMessage:
    given Encoder[ApiMessage] = deriveEncoder
  final case class ApiError(code: String, field: String, message: String)
  object ApiError:
    given Encoder[ApiError] = deriveEncoder

  val default: PartialFunction[Throwable, (Status, ApiMessage)] =
    case ve: ValidationError =>
      (Status.UnprocessableContent, ApiMessage(ve.code, ve.message, ve.errors.map(e => ApiError(e.code, e.field, e.message.getOrElse("")))))
    case pf: ParseFailure =>
      (Status.BadRequest, ApiMessage("parse_failure", pf.message))
    case mmbf: MalformedMessageBodyFailure =>
      (Status.BadRequest, ApiMessage("invalid_request", mmbf.message))
    case imbf: InvalidMessageBodyFailure => handleInvalidMessageBodyFailure(imbf)
    case umtf: UnsupportedMediaTypeFailure =>
      (Status.UnsupportedMediaType, ApiMessage("invalid_request", umtf.message))
    case t =>
      logger.error(t)(s"Service raised an unexpected error: ${t.toString}")
      (Status.InternalServerError, ApiMessage("server_error", t.toString))

  def defaultErrorHandler[F[_] : Sync]: PartialFunction[Throwable, F[Response[F]]] =
    errorHandlerWithFallback(default)

  def errorHandler[F[_] : Sync](pf: PartialFunction[Throwable, (Status, ApiMessage)]): PartialFunction[Throwable, F[Response[F]]] =
    errorHandlerWithFallback(pf.orElse(default))

  private def errorHandlerWithFallback[F[_] : Sync](pf: PartialFunction[Throwable, (Status, ApiMessage)]): PartialFunction[Throwable, F[Response[F]]] =
    pf.andThen { case (status, message) =>
      Response[F](status).withEntity(message).pure[F]
    }

  private def handleInvalidMessageBodyFailure(imbf: InvalidMessageBodyFailure): (Status, ApiMessage) =
    val msg = imbf.cause match
      case Some(df: DecodingFailure) =>
        val error = failureToApiError(df)
        ApiMessage("validation_error", "validation failed", List(error))
      case Some(dfs: DecodingFailures) =>
        val errors = dfs.failures.map(failureToApiError).toList
        ApiMessage("validation_error", "validation failed", errors)
      case _ => ApiMessage("validation_error", imbf.message)
    (Status.UnprocessableContent, msg)

  private def failureToApiError(df: DecodingFailure): ApiError =
    ApiError("field_error", failureToFieldPath(df), df.message)

  private def failureToFieldPath(df: DecodingFailure): String =
    val path = CursorOp.opsToPath(df.history)
    if path.startsWith(".") then path.replaceFirst(".", "") else path
