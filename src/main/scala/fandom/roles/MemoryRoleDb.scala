package fandom
package roles

import cats.data._
import cats.syntax.flatMap._

object MemoryRoleDb extends RoleDb[State[List[Grant], ?]] {
  type S[A] = State[List[Grant], A]

  def findRolesForUser(userId: UserId, scopesFilter: List[Scope]): S[List[Role]] =
    for {
      grants ← findGrantsForUsers(NonEmptyList.of(userId), scopesFilter)
    } yield (grants.map(_.role))

  def findGrantsForUsers(
    users: NonEmptyList[UserId],
    scopesFilter: List[Scope]
  ): S[List[Grant]] =
    State.get[List[Grant]].map { s ⇒
      s.filter(role ⇒ users.toList.contains(role.userId))
        .filter(role ⇒ scopeMatchesOrNotFiltered(scopesFilter)(role))
    }

  def bulkDeleteRolesForUser(userId: UserId, scopesFilter: List[Scope]): S[Int] =
    for {
      before ← State.get[List[Grant]]
      after = before.filterNot(grantMatches(userId, scopesFilter))
      _ ← State.set(after)
    } yield (before.size - after.size)

  def addRolesForUser(userId: UserId, add: List[Role]): S[Unit] =
    deleteRolesForUser(userId, add) >>
      State.modify(_ ++ add.map(Grant(userId, _)))

  def deleteRolesForUser(userId: UserId, delete: List[Role]): S[Unit] =
    for {
      before ← State.get[List[Grant]]
      after = before.filterNot {
        grant ⇒ grant.userId == userId & delete.contains(grant.role)
      }
      _ ← State.set(after)
    } yield (())

  def grantMatches(userId: UserId, scopesFilter: List[Scope])(grant: Grant): Boolean =
    grant.userId == userId && scopeMatchesOrNotFiltered(scopesFilter)(grant)

  def scopeMatchesOrNotFiltered(scopesFilter: List[Scope])(grant: Grant): Boolean =
    (scopesFilter.isEmpty || scopesFilter.contains(grant.role.scope))
}
