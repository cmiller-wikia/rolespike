package fandom
package roles

trait RoleDb[F[_]] {
  def findRolesForUser(userId: UserId, scopesFilter: List[Scope] = List.empty): F[List[Role]]

  def bulkDeleteRolesForUser(userId: UserId, scopesFilter: List[Scope] = List.empty): F[Int]

  def addRolesForUser(userId: UserId, add: List[Role]): F[Unit]

  def deleteRolesForUser(userId: UserId, delete: List[Role]): F[Unit]
}
