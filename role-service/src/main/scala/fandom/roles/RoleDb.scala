package fandom
package roles

import cats.data.NonEmptyList

trait RoleDb[F[_]] {
  def findGrantsForUsers(users: NonEmptyList[UserId], scopesFilter: List[Scope] = List.empty): F[List[Grant]]

  def findRolesForUser(userId: UserId, scopesFilter: List[Scope] = List.empty): F[List[Role]]

  def bulkDeleteRolesForUser(userId: UserId, scopesFilter: List[Scope] = List.empty): F[Int]

  def addRolesForUser(userId: UserId, add: List[Role]): F[Unit]

  def deleteRolesForUser(userId: UserId, delete: List[Role]): F[Unit]
}
