package fandom

import cats._
import cats.syntax.flatMap._
import doobie.imports._
import fs2._
import fs2.interop.cats._
import apitests._

trait DoobieMySQLOps {
  val connectionUri =
    sys.env.get("MYSQL_CONNECTION_URI")
      .getOrElse("jdbc:mysql://localhost:3306/roletestdb?useSSL=false")

  val username =
    sys.env.get("MYSQL_USERNAME")
      .getOrElse("root")

  val password =
    sys.env.get("MYSQL_PASSWORD")
      .getOrElse("secret")

  val xa =
    Transactor.fromDriverManager[Task](
      "com.mysql.cj.jdbc.Driver",
      connectionUri,
      username,
      password
    )

  def connectionIOToTaskWithInitializer(init: ConnectionIO[_]): (ConnectionIO ~> Task) =
    new (ConnectionIO ~> Task) {
      def apply[A](fa: ConnectionIO[A]): Task[A] = (init >> fa).transact(xa)
    }

  def runInMysqlAfter(init: ConnectionIO[_]): (ConnectionIO ~> Id) =
    taskToId compose connectionIOToTaskWithInitializer(init)
}

object doobiemysqltest extends DoobieMySQLOps
