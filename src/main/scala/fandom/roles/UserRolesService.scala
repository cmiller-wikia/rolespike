package fandom
package roles

import fs2.interop.cats._
import cats._
import cats.instances.unit._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._
import io.circe.syntax._
import web._
import codecs._

trait UserRolesService[F[_]] {
  val DB: RoleDb[F]
  val T: F ~> WebOp
  implicit val F: Monad[F]

  //format: OFF
  val service = HttpService {
    case req @    GET -> Root / "roles" / "users" / userId =>
      run(findRolesForUser(UserId(userId)))(req)
    case req @ DELETE -> Root / "roles" / "users" / userId =>
      run(deleteRolesForUser(UserId(userId)))(req)
    case req @   POST -> Root / "roles" / "users" / userId =>
      run(updateRolesForUser(UserId(userId)))(req)
  }
  //format: ON

  def findRolesForUser(userId: UserId): WebService =
    for {
      scopes ← multiParam("scope", Scope(_))
      roles ← T(DB.findRolesForUser(userId, scopes))
      result ← liftTask(Ok(roles.asJson))
    } yield (result)

  def deleteRolesForUser(userId: UserId): WebService =
    for {
      scopes ← multiParam("scope", Scope(_))
      roles ← T(DB.bulkDeleteRolesForUser(userId, scopes))
      result ← liftTask(NoContent())
    } yield (result)

  def updateRolesForUser(userId: UserId): WebService =
    for {
      patch ← bodyAs(jsonOf[Patch])
      _ ← validatePatch(patch)
      updatedRoles ← T(updateRolesInDb(userId, patch.add, patch.remove))
      result ← liftTask(Ok(updatedRoles.asJson))
    } yield (result)

  def updateRolesInDb(
    userId: UserId,
    add: List[Role],
    remove: List[Role]): F[List[Role]] =
    for {
      _ ← DB.addRolesForUser(userId, add)
      _ ← DB.deleteRolesForUser(userId, remove)
      updatedRoles ← DB.findRolesForUser(userId)
    } yield (updatedRoles)

  def validatePatch(patch: Patch): WebOp[Unit] =
    if (patch.add.isEmpty && patch.remove.isEmpty)
      sendError(
        UnprocessableEntity(
          "Patch must contain at least one role to add or remove"))
    else
      ().pure[WebOp]
}

object UserRolesService {
  def service[F[_]](DDB: RoleDb[F], TT: F ~> WebOp)(implicit FF: Monad[F]): HttpService =
    new UserRolesService[F] {
      val T = TT
      val DB = DDB
      val F = FF
    }.service
}
