package fandom

import io.circe._
import io.circe.parser._

import cats.data._
import cats.syntax.validated._

trait CirceMatchers {
  /**
   * Validator for "api compatibility" between two Json values - that is, an API client that expects the "api"
   * value should be satisfied by the "candidate" value.
   *
   * To explicitly test that some property is missing, it must be set to 'null' in the api value.
   */
  def conformsTo(api: Json)(candidate: Json): Either[String, Json] = {
    def _findDifferences(path: String, left: Json, right: Json): List[String] = {
      def writePath: String = if (path == "") "<root>" else path

      def error(msg: String): List[String] = List("at path " + writePath + " " + msg)

      def typeError(): List[String] = error(right.name + " is not " + left.name)

      def compareScalar[A](f: (Json ⇒ Option[A]))(lv: A): List[String] =
        f(right).map { rv ⇒
          if (lv == rv)
            List.empty
          else
            error(rv.toString + " is not " + lv.toString)
        }.getOrElse(typeError)

      def compareNull(): List[String] =
        if (right.isNull) List.empty else error("is not null")

      def compareArray(l: Vector[Json]): List[String] =
        right.asArray.map(r ⇒
          if (l.size != r.size) {
            error("should be length " + l.size + " but is " + r.size)
          } else {
            Stream.from(0).zip(l zip r) // -> Stream[(Int (Json, Json))]
              .foldLeft(List.empty[String])(
                (errs, item) ⇒ errs ++ (item match {
                  case (idx, (ll, rr)) ⇒ _findDifferences(path + "[" + idx + "]/", ll, rr)
                }))
          }).getOrElse(typeError)

      def compareObject(l: JsonObject): List[String] =
        right.asObject.map(r ⇒
          l.toList.flatMap {
            case (key, value) ⇒ _findDifferences(path + "/" + key, value, r(key).getOrElse(Json.Null))
          }).getOrElse(typeError)

      left.fold(
        compareNull,
        compareScalar(_.asBoolean),
        compareScalar(_.asNumber),
        compareScalar(_.asString),
        compareArray,
        compareObject)
    }

    _findDifferences("", api, candidate) match {
      case Nil ⇒ Right(candidate)
      case errs ⇒ Left(errs.mkString(", "))
    }
  }

  def validJson(expected: String): ValidatedNel[String, Json] = parse(expected).fold(
    _.message.invalidNel,
    _.valid)

  def beJson = ValidationMatchers.validateWith(validJson)

  def conformTo(expected: Json) = EitherMatchers.beRight(conformsTo(expected))
}

object CirceMatchers extends CirceMatchers
