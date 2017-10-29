package fandom
package roles

import io.circe._
import io.circe.generic.semiauto._

case class Scope(value: String)

case class UserId(value: String)

case class Role(name: String, scope: Scope)

case class Grant(userId: UserId, role: Role)

case class Patch(add: List[Role], remove: List[Role])

object codecs {
  implicit val encodeScope: Encoder[Scope] = new Encoder[Scope] {
    def apply(scope: Scope): Json = Json.fromString(scope.value)
  }

  implicit val decodeScope: Decoder[Scope] = new Decoder[Scope] {
    final def apply(c: HCursor): Decoder.Result[Scope] =
      c.as[String].map(Scope(_))
  }

  implicit val encodeRole: Encoder[Role] = deriveEncoder[Role]
  implicit val decodeRole: Decoder[Role] = deriveDecoder[Role]

  implicit val encodePatch: Encoder[Patch] = deriveEncoder[Patch]

  implicit val decodePatch: Decoder[Patch] = new Decoder[Patch] {
    def attemptParseListAt(name: String)(c: HCursor): Decoder.Result[List[Role]] = {
      val field = c.downField(name)
      if (!field.succeeded)
        Right(List.empty)
      else
        field.as[List[Role]]
    }

    final def apply(c: HCursor): Decoder.Result[Patch] =
      for {
        add <- attemptParseListAt("add")(c)
        remove <- attemptParseListAt("remove")(c)
      } yield (Patch(add, remove))
  }
}
