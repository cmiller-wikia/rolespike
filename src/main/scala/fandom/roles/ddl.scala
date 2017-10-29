package fandom
package roles

import doobie.imports._
import cats.instances.list._

trait DdlOps {
  object grants {
    val dropTable: Update0 =
      sql"""
			  DROP TABLE IF EXISTS grants
			""".update

    val createTable: Update0 =
      sql"""
			  CREATE TABLE grants (
					user_id        VARCHAR(100)     NOT NULL,
					role_name      VARCHAR(100)     NOT NULL,
					scope_name     VARCHAR(100)     NOT NULL
				)""".update

    def insertFixtures(fixtures: List[Grant]): ConnectionIO[Int] =
      DoobieRoleDb.grantInserter.updateMany(
        fixtures.map {
          (grant: Grant) => (grant.userId.value, grant.role.name, grant.role.scope.value)
        }
      )
  }
}

object ddl extends DdlOps
