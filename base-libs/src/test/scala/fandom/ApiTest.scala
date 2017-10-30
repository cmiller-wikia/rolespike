package fandom

import cats._
import cats.data.State
import fs2.Task

trait ApiTestOps extends TransformOps {
  def stateToId[S](initialState: S): State[S, ?] ~> Id =
    new (State[S, ?] ~> Id) {
      def apply[A](fa: State[S, A]): Id[A] =
        fa.run(initialState).value._2
    }

  val taskToId: Task ~> Id = new (Task ~> Id) {
    def apply[A](fa: Task[A]): Id[A] =
      fa.unsafeValue.getOrElse(throw new IllegalStateException("Task attempted to run asynchronously"))
  }
}

object apitests extends ApiTestOps
