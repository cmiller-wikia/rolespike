package fandom

import cats._
import cats.syntax.either._
import Predef.ArrowAssoc
import Predef.{ identity => id, ??? }

/**
 * A program takes some initial state I, accumulates output O, and exits with either an error or
 * a return value.
 */
case class ProgramT[F[_], I, O, E, A](val run: I => F[(O, E \/ A)]) {
  def map[B](f: A => B)(implicit F: Functor[F]): ProgramT[F, I, O, E, B] =
    ProgramT { i =>
      F.map(run(i)) {
        case (o, ea) => (o, ea.map(f))
      }
    }

  def flatMap[B](f: A => ProgramT[F, I, O, E, B])(implicit F: Monad[F], W: Semigroup[O]): ProgramT[F, I, O, E, B] =
    ProgramT { i =>
      F.flatMap(run(i)) {
        case (o, ea) => ea match {
          case err @ Left(_) => F.pure(o -> err.rightCast[B])
          case Right(a) => F.map(f(a).run(i)) {
            case (o2, eb) => (W.combine(o, o2), eb)
          }
        }
      }
    }

  def translate[II, OO, EE](fi: II => I, fo: O => OO, fe: E => EE)(implicit F: Functor[F]): ProgramT[F, II, OO, EE, A] =
    ProgramT { ii =>
      F.map(run(fi(ii))) {
        case (o, ea) => (fo(o), ea.leftMap(fe))
      }
    }

  def translateErrorM[EE](f: E => F[EE])(implicit F: Monad[F]): ProgramT[F, I, O, EE, A] =
    ProgramT { i =>
      F.flatMap(run(i)) {
        case (om, ea) => ea match {
          case Left(e) => F.map(f(e))(ee => (om -> Left(ee)))
          case succ @ Right(_) => F.pure(om -> succ.leftCast[EE])
        }
      }
    }

  def translateError[EE](f: E => EE)(implicit F: Functor[F]): ProgramT[F, I, O, EE, A] =
    translate(id, id, f)

  def translateInput[II](f: II => I)(implicit F: Functor[F]): ProgramT[F, II, O, E, A] =
    translate(f, id, id)

  def translateOutput[OO](f: O => OO)(implicit F: Functor[F]): ProgramT[F, I, OO, E, A] =
    translate(id, f, id)
}

object ProgramT {
  def monad[F[_], I, O, E](implicit F: Monad[F], O: Monoid[O]) = new ProgramTMonadInstances[F, I, O, E] {
    def M = F
    def D = O
  }

  def transLift[I, O, E](implicit O: Monoid[O]) = new ProgramTTransLift[I, O, E] {
    def D = O
  }
}

trait ProgramTMonad[F[_], I, O, E] extends Monad[ProgramT[F, I, O, E, ?]] {
  def M: Monad[F]
  def D: Monoid[O]

  override def pure[A](a: A): ProgramT[F, I, O, E, A] =
    ProgramT(i => M.pure(D.empty -> a.asRight))

  override def flatMap[A, B](fa: ProgramT[F, I, O, E, A])(f: A => ProgramT[F, I, O, E, B]): ProgramT[F, I, O, E, B] =
    fa.flatMap(f)(M, D)

  override def map[A, B](fa: ProgramT[F, I, O, E, A])(f: A => B): ProgramT[F, I, O, E, B] =
    fa.map(f)(M)

  // And this is where I'm really happy I have law tests written for me
  override def tailRecM[A, B](a: A)(f: A => ProgramT[F, I, O, E, A \/ B]): ProgramT[F, I, O, E, B] = {
    def step(i: I)(oa: (O, A)): F[(O, A) \/ (O, E \/ B)] = {
      M.map(f(oa._2).run(i)) {
        case (o, Left(err)) => Right(D.combine(oa._1, o) -> Left(err))
        case (o, Right(Left(a))) => Left(D.combine(oa._1, o) -> a)
        case (o, Right(Right(b))) => Right(D.combine(oa._1, o) -> b.asRight)
      }
    }

    ProgramT { i => M.tailRecM(D.empty -> a)(step(i)) }
  }
}

trait ProgramTMonadReader[F[_], I, O, E] extends MonadReader[ProgramT[F, I, O, E, ?], I] {
  def M: Monad[F]
  def D: Monoid[O]

  def ask: ProgramT[F, I, O, E, I] =
    ProgramT { i => M.pure(D.empty -> i.asRight) }

  def local[A](f: I => I)(fa: ProgramT[F, I, O, E, A]): ProgramT[F, I, O, E, A] =
    ProgramT { i => fa.run(f(i)) }
}

trait ProgramTMonadWriter[F[_], I, O, E] extends MonadWriter[ProgramT[F, I, O, E, ?], O] {
  def M: Monad[F]

  def writer[A](ao: (O, A)): ProgramT[F, I, O, E, A] =
    ProgramT(i => M.pure(ao._1 -> ao._2.asRight))

  def listen[A](fa: ProgramT[F, I, O, E, A]): ProgramT[F, I, O, E, (O, A)] =
    ProgramT { i =>
      M.map(fa.run(i)) {
        case (o, a) => (o -> a.map(o -> _))
      }
    }

  // I don't know what a working instance of this looks like, and can't find any tests
  // or laws that would tell me, so I'm going to chicken out.
  def pass[A](fa: ProgramT[F, I, O, E, (O => O, A)]): ProgramT[F, I, O, E, A] = ???
}

trait ProgramTMonadError[F[_], I, O, E] extends MonadError[ProgramT[F, I, O, E, ?], E] {
  def M: Monad[F]
  def D: Monoid[O]

  def raiseErrorM[A](fe: F[E]): ProgramT[F, I, O, E, A] =
    ProgramT { i =>
      M.map(fe) { e => (D.empty -> e.asLeft) }
    }

  def raiseError[A](e: E): ProgramT[F, I, O, E, A] =
    ProgramT { i => M.pure(D.empty -> e.asLeft) }

  def handleErrorWith[A](fa: ProgramT[F, I, O, E, A])(f: E => ProgramT[F, I, O, E, A]): ProgramT[F, I, O, E, A] =
    ProgramT { i =>
      val prerun = fa.run(i)

      M.flatMap(prerun) {
        case (o, ea) => ea match {
          case Left(e) => M.map(f(e).run(i)) {
            case (oo, ea2) => (D.combine(o, oo) -> ea2)
          }
          case _ => prerun
        }
      }
    }
}

trait ProgramTTransLift[I, O, E] extends TransLift[λ[(A[_], B) => ProgramT[A, I, O, E, B]]] {
  def D: Monoid[O]

  type TC[M[_]] = Monad[M]

  override def liftT[M[_], A](ma: M[A])(implicit M: TC[M]): ProgramT[M, I, O, E, A] = {
    ProgramT { i => M.map(ma)(a => (D.empty, a.asRight)) }
  }
}

trait ProgramTMonadInstances[F[_], I, O, E]
  extends ProgramTMonad[F, I, O, E]
  with ProgramTMonadReader[F, I, O, E]
  with ProgramTMonadWriter[F, I, O, E]
  with ProgramTMonadError[F, I, O, E]
