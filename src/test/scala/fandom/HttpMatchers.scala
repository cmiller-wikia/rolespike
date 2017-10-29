package fandom

import cats._
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.data._
import io.circe._
import org.http4s._
import org.http4s.circe._
import fs2.Task
import fs2.interop.cats._
import org.scalatest.matchers._

trait HttpMatchers {
  type ETS[A] = EitherT[Task, String, A]
  val ME = MonadError[ETS, String]

  def liftTask[A](ta: Task[A]): ETS[A] = EitherT.right(ta)

  def requestHandled: MaybeResponse => ETS[Response] =
    mr => EitherT.fromEither(mr.toOption.toRight("Service did not handle request"))

  def returns[A](implicit decoder: EntityDecoder[A]): Response => ETS[A] =
    _.attemptAs[A]
      .leftMap(_.message)

  def hasStatus(status: Status): Response => ETS[Response] =
    response =>
      if (response.status == status)
        response.pure[ETS]
      else
        ME.raiseError("Http status expected: " + status + " actual: " + response.status)

  def jsonConformsTo(expected: Json): Json => ETS[Json] =
    actual => EitherT.fromEither(CirceMatchers.conformsTo(expected)(actual))

  def runsSynchronously[A](ets: ETS[A]): Either[String, A] =
    ets.value
      .unsafeAttemptValue
      .toRight("Service did not complete synchronously")
      .flatMap(_.leftMap(_.getMessage))
      .flatten

  def validatesAgainst[A](f: MaybeResponse => ETS[A]): Matcher[Task[MaybeResponse]] =
    EitherMatchers.beRight(task => runsSynchronously(liftTask(task) >>= f))

  // There has to be a cleaner way of composing A => F[B] than this, but I can't find it in
  // cats.
  def respondWithConformingJson(expected: Json): Matcher[Task[MaybeResponse]] =
    validatesAgainst((
      Kleisli(requestHandled) andThen
      hasStatus(Status.Ok) andThen
      returns[Json] andThen
      jsonConformsTo(expected)
    ).apply)

  def respondWithStatus(status: Status): Matcher[Task[MaybeResponse]] =
    validatesAgainst((
      (Kleisli(requestHandled) andThen hasStatus(status)).apply
    ))
}

object HttpMatchers extends HttpMatchers
