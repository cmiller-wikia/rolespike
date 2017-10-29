package fandom

import cats._
import cats.instances.unit._
import cats.instances.int._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.either._
import cats.syntax.flatMap._
import cats.laws.discipline._
import cats.laws.discipline.eq._
import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.Arbitrary
import org.scalatest.{ FunSuite, Matchers }

class ProgramTSpec extends FunSuite with Matchers with Discipline {
  type TestProgram[A] = ProgramT[Eval, Int, String, String, A]
  implicit val M = ProgramT.monad[Eval, Int, String, String]

  implicit def arbProgramT[F[_], I, O: Monoid, E, A: Arbitrary](implicit AO: Arbitrary[O], AEA: Arbitrary[E \/ A], F: Applicative[F]): Arbitrary[ProgramT[F, I, O, E, A]] =
    Arbitrary(
      for {
        o <- AO.arbitrary
        ea <- AEA.arbitrary
      } yield (ProgramT(i => F.pure((o, ea))))
    )

  implicit def eqTestProgram[A](implicit A: Arbitrary[Int], E: Eq[Eval[(String, String \/ A)]]): Eq[TestProgram[A]] =
    Eq.by[TestProgram[A], Int => Eval[(String, String \/ A)]] { program => i => program.run(i) }

  // includes monad tests
  checkAll("ProgramT[Int]", MonadErrorTests[TestProgram, String].monadError[Int, Int, Int])

  test("writes accumulate until an error") {
    val out = (M.tell("One") >> M.tell("Two") >> M.raiseError("Fail") >> M.tell("Three")).run(10)

    out.value shouldBe (("OneTwo", Left("Fail")))
  }

  test("writes resume after an error is recovered") {
    val failing = M.tell("One") >> M.raiseError("Fail") >> M.tell("Two")
    val recovered = M.handleError(failing)(_ => ()) >> M.tell("Three")
    recovered.run(10).value shouldBe (("OneThree", Right(())))
  }
}
