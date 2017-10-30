package fandom
package roles

import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.flatMap._
import doobie.imports._

object DoobieRoleDb extends RoleDb[ConnectionIO] {
  def findRolesForUser(userId: UserId, scopesFilter: List[Scope]): ConnectionIO[List[Role]] =
    for {
      grants ← findGrantsForUsers(NonEmptyList.of(userId), scopesFilter)
    } yield (grants.map(_.role))

  def findGrantsForUsers(
    userIds: NonEmptyList[UserId],
    scopesFilter: List[Scope]
  ): ConnectionIO[List[Grant]] =
    rolesQuery(userIds, scopesFilter)
      .query[(String, String, String)]
      .map {
        case (userId, roleName, scopeName) ⇒
          Grant(UserId(userId), Role(roleName, Scope(scopeName)))
      }
      .list

  def bulkDeleteRolesForUser(userId: UserId, scopesFilter: List[Scope]): ConnectionIO[Int] =
    deleteByUserScopeQuery(userId, scopesFilter).update.run

  def addRolesForUser(userId: UserId, add: List[Role]): ConnectionIO[Unit] =
    deleteRolesForUser(userId, add) >>
      grantInserter.updateMany(add.map {
        role ⇒ (userId.value, role.name, role.scope.value)
      }).map(_ ⇒ ())

  def deleteRolesForUser(userId: UserId, delete: List[Role]): ConnectionIO[Unit] =
    grantDeleter.updateMany(delete.map {
      role ⇒ (userId.value, role.name, role.scope.value)
    }).map(_ ⇒ ())

  def rolesQuery(userIds: NonEmptyList[UserId], scopes: List[Scope]) =
    sql"""
      SELECT user_id, role_name, scope_name
      FROM grants
      WHERE """ ++
      Fragments.in(fr"user_id", userIds) ++
      optionalScopesFilter(scopes)

  def deleteByUserScopeQuery(userId: UserId, scopes: List[Scope]) =
    sql"""
      DELETE FROM grants
      WHERE user_id = ${userId.value}
      """ ++ optionalScopesFilter(scopes)

  val grantDeleter =
    Update[(String, String, String)](
      """DELETE FROM grants
         WHERE user_id = ?
           AND role_name = ?
           AND scope_name = ?"""
    )

  val grantInserter =
    Update[(String, String, String)](
      """INSERT INTO grants(
           user_id, role_name, scope_name
         ) VALUES ( ?, ?, ?)"""
    )

  def optionalScopesFilter(scopes: List[Scope]) =
    NonEmptyList.fromList(scopes.map(_.value))
      .map(
        scopeNames ⇒ fr"AND" ++ Fragments.in(fr"scope_name", scopeNames)
      )
      .getOrElse(Fragment.empty)
}
