package fandom
package roles

import cats._
import fs2.{ Stream, Task }
import fs2.interop.cats._
import org.http4s.server.blaze._
import org.http4s.util.StreamApp
import doobie.imports._
import web._

trait TransientRolesApp {
  import java.sql._

  val xa = Transactor.fromDriverManager[Task]("org.h2.Driver", "jdbc:h2:mem:db", "", "")

  def userRolesService =
    UserRolesService.service(
      DoobieRoleDb,
      roleDbTransformer
    )

  def rolesService =
    RolesService.service(
      DoobieRoleDb,
      roleDbTransformer
    )

  def roleDbTransformer = new (ConnectionIO ~> WebOp) {
    def apply[A](fa: ConnectionIO[A]): WebOp[A] = liftTask(fa.transact(xa))
  }

  // H2 only retains an in-memory db for so long as a connection is open to it.
  // So we create a connection and hold it for as long as the app is running.
  def keepAliveMemoryDb(app: Stream[Task, Nothing]): Stream[Task, Nothing] =
    Stream.bracket(initDb)(
      _ ⇒ app,
      conn ⇒ Task.delay { conn.close }
    )

  def initDb: Task[Connection] =
    for {
      conn ← openRawConnection
      _ ← createTables
    } yield (conn)

  def openRawConnection = xa.connect(xa.kernel)

  def createTables = ddl.grants.createTable.run.transact(xa)
}

object RolesApp extends StreamApp with TransientRolesApp {
  override def stream(args: List[String]): Stream[Task, Nothing] =
    keepAliveMemoryDb(
      BlazeBuilder
        .bindHttp(8080, "0.0.0.0")
        .mountService(userRolesService, "/")
        .mountService(rolesService, "/")
        .serve
    )
}
