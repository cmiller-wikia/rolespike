package fandom

import org.scalatest._, matchers._

/**
 * Allow us to derive Scalatest matchers from cats Validation so we have a way of composing test checks
 */
trait EitherMatchers {
  class IsRight[A, B](validate: A ⇒ Either[String, B]) extends Matcher[A] {
    def apply(actual: A): MatchResult = validate(actual).fold(
      err ⇒ MatchResult(
        false,
        err,
        "Unreachable message"
      ),
      succ ⇒ MatchResult(
        true,
        "Unreachable message",
        "Validation succeeded"
      )
    )

  }

  def beRight[A, B](validate: A ⇒ Either[String, B]) = new IsRight(validate)
}

object EitherMatchers extends EitherMatchers
