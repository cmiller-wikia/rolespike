package fandom
package roles

import cats.data.State
import cats.syntax.flatMap._

object MemoryRoleDb extends RoleDb[State[List[Grant], ?]] {
  type S[A] = State[List[Grant], A]

  def findRolesForUser(userId: UserId, scopesFilter: List[Scope]): S[List[Role]] =
    State.get[List[Grant]].map { s =>
      s.filter(grantMatches(userId, scopesFilter))
        .map(_.role)
    }

  def bulkDeleteRolesForUser(userId: UserId, scopesFilter: List[Scope]): S[Int] =
    for {
      before <- State.get[List[Grant]]
      after = before.filterNot(grantMatches(userId, scopesFilter))
      _ <- State.set(after)
    } yield (before.size - after.size)

  def addRolesForUser(userId: UserId, add: List[Role]): S[Unit] =
    deleteRolesForUser(userId, add) >>
      State.modify(_ ++ add.map(Grant(userId, _)))

  def deleteRolesForUser(userId: UserId, delete: List[Role]): S[Unit] =
    for {
      before <- State.get[List[Grant]]
      after = before.filterNot {
        grant => grant.userId == userId & delete.contains(grant.role)
      }
      _ <- State.set(after)
    } yield (())

  def grantMatches(userId: UserId, scopesFilter: List[Scope])(grant: Grant): Boolean =
    grant.userId == userId &&
      (scopesFilter.isEmpty || scopesFilter.contains(grant.role.scope))
}
