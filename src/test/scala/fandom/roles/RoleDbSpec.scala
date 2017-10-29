package fandom
package roles

import cats._
import cats.data._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest._
import Matchers._

trait RoleDbSpec { this: FreeSpec =>
  val defaultState: List[Grant] = fixtures.defaultGrants

  def roleDb[F[_]: Monad](E: RoleDb[F], T: List[Grant] => (F ~> Id)) = {
    "when looking up roles for a user" - {
      "returns nothing for a non-existent user" in {
        T(defaultState)(E.findRolesForUser(UserId("fred"))) shouldBe empty
      }

      "returns all roles for an existing user" in {
        T(defaultState)(E.findRolesForUser(UserId("bob"))) shouldBe List(
          Role("staff", Scope("global")),
          Role("discussions-moderator", Scope("wiki:831")),
          Role("discussions-helper", Scope("wiki:832"))
        )
      }

      "can be limited to a single scope" in {
        T(defaultState)(
          E.findRolesForUser(UserId("bob"), List(Scope("wiki:831")))
        ) shouldBe List(
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }

      "can be limited to multiple scopes" in {
        T(defaultState)(
          E.findRolesForUser(UserId("bob"), List(Scope("wiki:831"), Scope("global")))
        ) shouldBe List(
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }

      "returns nothing if limited to a non-matching scope" in {
        T(defaultState)(
          E.findRolesForUser(UserId("bob"), List(Scope("wiki:830")))
        ) shouldBe empty
      }

      "handles limits that include non-existent scopes" in {
        T(defaultState)(
          E.findRolesForUser(
            UserId("bob"),
            List(Scope("wiki:831"), Scope("global"), Scope("cheese"))
          )
        ) shouldBe List(
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }

      "returns everything if filter list is empty" in {
        T(defaultState)(
          E.findRolesForUser(UserId("bob"), List.empty)
        ) shouldBe List(
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }
    }

    "when looking up roles for multiple users" - {
      def userIds(first: String, rest: String*) =
        NonEmptyList.of(first, rest: _*).map(UserId(_))

      def scopes(ss: String*) = ss.toList.map(Scope(_))

      "handles case when no users exist" in {
        T(defaultState)(
          E.findGrantsForUsers(userIds("carol", "betty"))
        ) shouldBe empty
      }

      "handles case where users exist but scopes don't" in {
        T(defaultState)(
          E.findGrantsForUsers(
            userIds("bob", "harold"),
            scopes("wiki:844", "wiki:855")
          )
        ) shouldBe empty
      }

      "handles single user no scope" in {
        T(defaultState)(
          E.findGrantsForUsers(
            userIds("bob")
          )
        ).toList should contain only (
            Grant(UserId("bob"), Role("staff", Scope("global"))),
            Grant(UserId("bob"), Role("discussions-moderator", Scope("wiki:831"))),
            Grant(UserId("bob"), Role("discussions-helper", Scope("wiki:832")))
          )
      }

      "handles multi user no scope" in {
        T(defaultState)(
          E.findGrantsForUsers(
            userIds("bob", "harold")
          )
        ).toList should contain only (
            Grant(UserId("bob"), Role("staff", Scope("global"))),
            Grant(UserId("bob"), Role("discussions-moderator", Scope("wiki:831"))),
            Grant(UserId("bob"), Role("discussions-helper", Scope("wiki:832"))),
            Grant(UserId("harold"), Role("discussions-helper", Scope("wiki:831")))
          )
      }

      "handles multi user with scope" in {
        T(defaultState)(
          E.findGrantsForUsers(
            userIds("bob", "harold"),
            scopes("wiki:831", "wiki:832")
          )
        ).toList should contain only (
            Grant(UserId("bob"), Role("discussions-moderator", Scope("wiki:831"))),
            Grant(UserId("bob"), Role("discussions-helper", Scope("wiki:832"))),
            Grant(UserId("harold"), Role("discussions-helper", Scope("wiki:831")))
          )
      }

      "handles mix of existing and non-existent things" in {
        T(defaultState)(
          E.findGrantsForUsers(
            userIds("bob", "carol", "harold"),
            scopes("wiki:842", "wiki:831")
          )
        ).toList should contain only (
            Grant(UserId("bob"), Role("discussions-moderator", Scope("wiki:831"))),
            Grant(UserId("harold"), Role("discussions-helper", Scope("wiki:831")))
          )
      }
    }

    "when bulk deleting roles for a user" - {
      "returns 0 if there are no grants to delete" in {
        T(defaultState)(
          E.bulkDeleteRolesForUser(UserId("fred"), List.empty)
        ) shouldBe 0
      }

      "deletes all grants for the user if no scopes are filtered" in {
        T(defaultState)(
          for {
            deleted <- E.bulkDeleteRolesForUser(UserId("bob"))
            remaining <- E.findRolesForUser(UserId("bob"))
          } yield ((deleted, remaining))
        ) shouldBe ((3, List.empty))
      }

      "deletes some grants for the user if a scope is filtered" in {
        T(defaultState)(
          for {
            deleted <- E.bulkDeleteRolesForUser(UserId("bob"), List(Scope("wiki:831")))
            remaining <- E.findRolesForUser(UserId("bob"))
          } yield ((deleted, remaining))
        ) shouldBe ((1, List(
            Role("staff", Scope("global")),
            Role("discussions-helper", Scope("wiki:832"))
          )))
      }
    }

    "when deleting roles for a user" - {
      "does nothing for an empty list" in {
        T(defaultState)(
          E.deleteRolesForUser(UserId("harold"), List.empty) >>
            E.findRolesForUser(UserId("harold"))
        ) shouldBe List(
            Role("discussions-helper", Scope("wiki:831"))
          )
      }

      "does nothing for a user with no roles" in {
        T(defaultState)(
          E.deleteRolesForUser(UserId("fred"), List(Role("foo", Scope("global"))))
        ) shouldBe (())
      }

      "does nothing for a role that does not exist" in {
        T(defaultState)(
          E.deleteRolesForUser(UserId("bob"), List(Role("foo", Scope("global")))) >>
            E.findRolesForUser(UserId("bob"))
        ) shouldBe List(
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }

      "deletes a single grant that does exist" in {
        T(defaultState)(
          E.deleteRolesForUser(UserId("bob"), List(Role("staff", Scope("global")))) >>
            E.findRolesForUser(UserId("bob"))
        ) shouldBe List(
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }

      "deletes mix of existing and non-existing grants" in {
        T(defaultState)(
          E.deleteRolesForUser(
            UserId("bob"),
            List(
              Role("staff", Scope("global")),
              Role("discussions-moderator", Scope("wiki:832")),
              Role("discussions-helper", Scope("wiki:832"))
            )
          ) >>
            E.findRolesForUser(UserId("bob"))
        ) shouldBe List(
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }
    }

    "when adding roles for a user" - {

      "does nothing for an empty list" in {
        T(defaultState)(
          E.addRolesForUser(UserId("harold"), List.empty) >>
            E.findRolesForUser(UserId("harold"))
        ) shouldBe List(
            Role("discussions-helper", Scope("wiki:831"))
          )
      }

      "adds roles for a user that does not exist" in {
        T(defaultState)(
          E.addRolesForUser(UserId("greg"), List(
            Role("foo", Scope("global")),
            Role("bar", Scope("global"))
          )) >>
            E.findRolesForUser(UserId("greg"))
        ) shouldBe List(
            Role("foo", Scope("global")),
            Role("bar", Scope("global"))
          )
      }

      "adds roles for a user that exists" in {
        T(defaultState)(
          E.addRolesForUser(UserId("harold"), List(
            Role("vstf", Scope("global")),
            Role("discussions-peon", Scope("wiki:831"))
          )) >>
            E.findRolesForUser(UserId("harold"))
        ).toList should contain only (
            Role("discussions-helper", Scope("wiki:831")),
            Role("vstf", Scope("global")),
            Role("discussions-peon", Scope("wiki:831"))
          )
      }

      "does nothing for roles that already exist" in {
        T(defaultState)(
          E.addRolesForUser(UserId("bob"), List(
            Role("staff", Scope("global"))
          )) >>
            E.findRolesForUser(UserId("bob"))
        ).toList should contain only (
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }

      "works for a mixture of existing and non-existing roles" in {
        T(defaultState)(
          E.addRolesForUser(UserId("bob"), List(
            Role("staff", Scope("global")),
            Role("vstf", Scope("global"))
          )) >>
            E.findRolesForUser(UserId("bob"))
        ).toList should contain only (
            Role("staff", Scope("global")),
            Role("vstf", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }
    }
  }
}

class MemoryRoleDbSpec extends FreeSpec with RoleDbSpec {
  "The memory role db" - {
    behave like roleDb(
      MemoryRoleDb,
      apitests.stateToId
    )
  }
}

class H2DoobieRoleDbSpec extends FreeSpec with RoleDbSpec {
  import cats.syntax.flatMap._
  import doobie.imports._

  def setup(initialState: List[Grant]): ConnectionIO[Int] =
    ddl.grants.createTable.run >> ddl.grants.insertFixtures(initialState)

  "The H2 role db" - {
    behave like roleDb(
      DoobieRoleDb,
      is => doobietest.runInH2After(setup(is))
    )
  }
}
