package fandom
package roles

import cats._
import cats.data._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest._
import Matchers._

trait RoleDbSpec { this: FreeSpec ⇒
  val defaultState: List[Grant] = fixtures.defaultGrants

  def roleDb[F[_]: Monad](DB: RoleDb[F], run: List[Grant] ⇒ (F ~> Id)) = {
    "when looking up roles for a user" - {
      "returns nothing for a non-existent user" in {
        run(defaultState)(DB.findRolesForUser(UserId("fred"))) shouldBe empty
      }

      "returns all roles for an existing user" in {
        run(defaultState)(DB.findRolesForUser(UserId("bob"))) shouldBe List(
          Role("staff", Scope("global")),
          Role("discussions-moderator", Scope("wiki:831")),
          Role("discussions-helper", Scope("wiki:832"))
        )
      }

      "can be limited to a single scope" in {
        run(defaultState)(
          DB.findRolesForUser(UserId("bob"), List(Scope("wiki:831")))
        ) shouldBe List(
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }

      "can be limited to multiple scopes" in {
        run(defaultState)(
          DB.findRolesForUser(UserId("bob"), List(Scope("wiki:831"), Scope("global")))
        ) shouldBe List(
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }

      "returns nothing if limited to a non-matching scope" in {
        run(defaultState)(
          DB.findRolesForUser(UserId("bob"), List(Scope("wiki:830")))
        ) shouldBe empty
      }

      "handles limits that include non-existent scopes" in {
        run(defaultState)(
          DB.findRolesForUser(
            UserId("bob"),
            List(Scope("wiki:831"), Scope("global"), Scope("cheese"))
          )
        ) shouldBe List(
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }

      "returns everything if filter list is empty" in {
        run(defaultState)(
          DB.findRolesForUser(UserId("bob"), List.empty)
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
        run(defaultState)(
          DB.findGrantsForUsers(userIds("carol", "betty"))
        ) shouldBe empty
      }

      "handles case where users exist but scopes don't" in {
        run(defaultState)(
          DB.findGrantsForUsers(
            userIds("bob", "harold"),
            scopes("wiki:844", "wiki:855")
          )
        ) shouldBe empty
      }

      "handles single user no scope" in {
        run(defaultState)(
          DB.findGrantsForUsers(
            userIds("bob")
          )
        ).toList should contain only (
            Grant(UserId("bob"), Role("staff", Scope("global"))),
            Grant(UserId("bob"), Role("discussions-moderator", Scope("wiki:831"))),
            Grant(UserId("bob"), Role("discussions-helper", Scope("wiki:832")))
          )
      }

      "handles multi user no scope" in {
        run(defaultState)(
          DB.findGrantsForUsers(
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
        run(defaultState)(
          DB.findGrantsForUsers(
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
        run(defaultState)(
          DB.findGrantsForUsers(
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
        run(defaultState)(
          DB.bulkDeleteRolesForUser(UserId("fred"), List.empty)
        ) shouldBe 0
      }

      "deletes all grants for the user if no scopes are filtered" in {
        run(defaultState)(
          for {
            deleted ← DB.bulkDeleteRolesForUser(UserId("bob"))
            remaining ← DB.findRolesForUser(UserId("bob"))
          } yield ((deleted, remaining))
        ) shouldBe ((3, List.empty))
      }

      "deletes some grants for the user if a scope is filtered" in {
        run(defaultState)(
          for {
            deleted ← DB.bulkDeleteRolesForUser(UserId("bob"), List(Scope("wiki:831")))
            remaining ← DB.findRolesForUser(UserId("bob"))
          } yield ((deleted, remaining))
        ) shouldBe ((1, List(
            Role("staff", Scope("global")),
            Role("discussions-helper", Scope("wiki:832"))
          )))
      }
    }

    "when deleting roles for a user" - {
      "does nothing for an empty list" in {
        run(defaultState)(
          DB.deleteRolesForUser(UserId("harold"), List.empty) followedBy
            DB.findRolesForUser(UserId("harold"))
        ) shouldBe List(
            Role("discussions-helper", Scope("wiki:831"))
          )
      }

      "does nothing for a user with no roles" in {
        run(defaultState)(
          DB.deleteRolesForUser(UserId("fred"), List(Role("foo", Scope("global"))))
        ) shouldBe (())
      }

      "does nothing for a role that does not exist" in {
        run(defaultState)(
          DB.deleteRolesForUser(UserId("bob"), List(Role("foo", Scope("global")))) followedBy
            DB.findRolesForUser(UserId("bob"))
        ) shouldBe List(
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }

      "deletes a single grant that does exist" in {
        run(defaultState)(
          DB.deleteRolesForUser(UserId("bob"), List(Role("staff", Scope("global")))) followedBy
            DB.findRolesForUser(UserId("bob"))
        ) shouldBe List(
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }

      "deletes mix of existing and non-existing grants" in {
        run(defaultState)(
          DB.deleteRolesForUser(
            UserId("bob"),
            List(
              Role("staff", Scope("global")),
              Role("discussions-moderator", Scope("wiki:832")),
              Role("discussions-helper", Scope("wiki:832"))
            )
          ) followedBy
            DB.findRolesForUser(UserId("bob"))
        ) shouldBe List(
            Role("discussions-moderator", Scope("wiki:831"))
          )
      }
    }

    "when adding roles for a user" - {

      "does nothing for an empty list" in {
        run(defaultState)(
          DB.addRolesForUser(UserId("harold"), List.empty) followedBy
            DB.findRolesForUser(UserId("harold"))
        ) shouldBe List(
            Role("discussions-helper", Scope("wiki:831"))
          )
      }

      "adds roles for a user that does not exist" in {
        run(defaultState)(
          DB.addRolesForUser(
            UserId("greg"),
            List(
              Role("foo", Scope("global")),
              Role("bar", Scope("global"))
            )
          ) followedBy
            DB.findRolesForUser(UserId("greg"))
        ) shouldBe List(
            Role("foo", Scope("global")),
            Role("bar", Scope("global"))
          )
      }

      "adds roles for a user that exists" in {
        run(defaultState)(
          DB.addRolesForUser(
            UserId("harold"),
            List(
              Role("vstf", Scope("global")),
              Role("discussions-peon", Scope("wiki:831"))
            )
          ) followedBy
            DB.findRolesForUser(UserId("harold"))
        ).toList should contain only (
            Role("discussions-helper", Scope("wiki:831")),
            Role("vstf", Scope("global")),
            Role("discussions-peon", Scope("wiki:831"))
          )
      }

      "does nothing for roles that already exist" in {
        run(defaultState)(
          DB.addRolesForUser(
            UserId("bob"),
            List(Role("staff", Scope("global")))
          ) followedBy
            DB.findRolesForUser(UserId("bob"))
        ).toList should contain only (
            Role("staff", Scope("global")),
            Role("discussions-moderator", Scope("wiki:831")),
            Role("discussions-helper", Scope("wiki:832"))
          )
      }

      "works for a mixture of existing and non-existing roles" in {
        run(defaultState)(
          DB.addRolesForUser(
            UserId("bob"),
            List(
              Role("staff", Scope("global")),
              Role("vstf", Scope("global"))
            )
          ) followedBy
            DB.findRolesForUser(UserId("bob"))
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
    ddl.grants.createTable.run followedBy ddl.grants.insertFixtures(initialState)

  "The H2 role db" - {
    behave like roleDb(
      DoobieRoleDb,
      is ⇒ doobietest.runInH2After(setup(is))
    )
  }
}

class MySqlRoleDbSpec extends FreeSpec with RoleDbSpec {
  import cats.syntax.flatMap._
  import doobie.imports._

  def setup(initialState: List[Grant]): ConnectionIO[Int] =
    ddl.grants.dropTable.run followedBy
      ddl.grants.createTable.run followedBy
      ddl.grants.insertFixtures(initialState)

  "The MySql role db" - {
    behave like roleDb(
      DoobieRoleDb,
      is ⇒ doobiemysqltest.runInMysqlAfter(setup(is))
    )
  }
}
