package fandom
package roles

import cats._
import cats.data._
import cats.instances.unit._
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._
import fs2.interop.cats._
import io.circe.syntax._
import web._
import codecs._
import scala.collection.immutable.Map
import Predef.ArrowAssoc

trait RolesService[F[_]] {
  val DB: RoleDb[F]
  val T: F ~> WebOp
  implicit val F: Monad[F]

  //format: OFF
  val service = HttpService {
    case req @    GET -> Root / "roles" => run(findRolesForUserGet)(req)
    case req @   POST -> Root / "roles" => run(findRolesForUserPost)(req)
  }
  //format: ON

  def findRolesForUserPost: WebService =
    for {
      form <- bodyAs[UrlForm]
      userIds = form.get("userId").map(UserId(_)).toList
      scopes = form.get("scope").map(Scope(_)).toList
      result <- findRolesForUser(userIds, scopes)
    } yield (result)

  def findRolesForUserGet: WebService =
    for {
      userIds <- multiParam("userId", UserId(_))
      scopes <- multiParam("scope", Scope(_))
      result <- findRolesForUser(userIds, scopes)
    } yield (result)

  def findRolesForUser(userIds: List[UserId], scopes: List[Scope]): WebService =
    for {
      grants <- NonEmptyList.fromList(userIds)
        .map(ids => T(DB.findGrantsForUsers(ids, scopes)))
        .getOrElse(sendError(BadRequest("At least one userId must be specified")))
      groupedRoles = ensureAllUsersPresent(userIds) ++ groupRoles(grants)
      result <- liftTask(Ok(groupedRoles.asJson))
    } yield result

  def groupRoles(grants: List[Grant]): Map[String, List[Role]] =
    grants.groupBy(_.userId.value).mapValues(_.map(_.role))

  def ensureAllUsersPresent(
    users: List[UserId]
  ): Map[String, List[Role]] =
    users.foldLeft(
      Map.empty[String, List[Role]]
    )(
      (roles, uid) => roles + (uid.value -> List.empty)
    )
}

object RolesService {
  def service[F[_]](DDB: RoleDb[F], TT: F ~> WebOp)(implicit FF: Monad[F]): HttpService =
    new RolesService[F] {
      val T = TT
      val DB = DDB
      val F = FF
    }.service
}
