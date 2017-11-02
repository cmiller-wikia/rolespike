package fandom

import cats._
import cats.data.State
import fs2.Task
import web._

trait TransformOps {
  def stateToTask[S](initialState: S): State[S, ?] ~> Task =
    new (State[S, ?] ~> Task) {
      val holder = new java.util.concurrent.atomic.AtomicReference[S](initialState)

      def apply[A](fa: State[S, A]): Task[A] =
        Task.delay {
          @annotation.tailrec
          def attempt: A = {
            val before = holder.get
            val (after, result) = fa.run(before).value
            if (holder.compareAndSet(before, after)) result else attempt
          }

          attempt
        }
    }

  def stateToWebOp[S](initialState: S): State[S, ?] ~> WebOp =
    taskToWebOp compose stateToTask(initialState)

  def idToWebOp: Id ~> WebOp =
    new (Id ~> WebOp) {
      def apply[A](a: Id[A]): WebOp[A] = WM.pure(a)

    }
}

object transforms extends TransformOps
