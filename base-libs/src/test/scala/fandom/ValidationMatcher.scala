package fandom

import org.scalatest._, matchers._

import cats.data._

/**
 * Allow us to derive Scalatest matchers from cats Validation so we have a way of composing test checks
 */
trait ValidationMatchers {
  class ValidatesWith[A, B](validate: A ⇒ ValidatedNel[String, B]) extends Matcher[A] {
    def apply(actual: A): MatchResult = validate(actual).fold(
      err ⇒ MatchResult(
        false,
        err.reduceLeft(_ + ", " + _),
        "Unreachable message"
      ),
      succ ⇒ MatchResult(
        true,
        "Unreachable message",
        "Validation succeeded"
      )
    )

  }

  def validateWith[A, B](validate: A ⇒ ValidatedNel[String, B]) = new ValidatesWith(validate)
}

object ValidationMatchers extends ValidationMatchers
