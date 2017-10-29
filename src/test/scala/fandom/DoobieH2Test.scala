package fandom

import cats._
import cats.syntax.flatMap._
import doobie.imports._
import fs2._
import fs2.interop.cats._
import apitests._

trait DoobieH2Ops {
  val xa = Transactor.fromDriverManager[Task]("org.h2.Driver", "jdbc:h2:mem:", "", "")

  def connectionIOToTaskWithInitializer(init: ConnectionIO[_]): (ConnectionIO ~> Task) =
    new (ConnectionIO ~> Task) {
      def apply[A](fa: ConnectionIO[A]): Task[A] = (init >> fa).transact(xa)
    }

  def runInH2After(init: ConnectionIO[_]): (ConnectionIO ~> Id) =
    taskToId compose connectionIOToTaskWithInitializer(init)
}

object doobietest extends DoobieH2Ops
