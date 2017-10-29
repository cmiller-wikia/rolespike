package fandom

import cats._
import cats.data._

object transformers {
  def stateToId[S](initialState: S): State[S, ?] ~> Id =
    new (State[S, ?] ~> Id) {
      def apply[A](s: State[S, A]): Id[A] = s.run(initialState).value._2
    }
}
