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
  implicit val encodeScope: Encoder[Scope] = wrappedStringEncoder[Scope](_.value)
  implicit val decodeScope: Decoder[Scope] = wrappedStringDecoder(Scope(_))

  implicit val encodeRole: Encoder[Role] = deriveEncoder[Role]
  implicit val decodeRole: Decoder[Role] = deriveDecoder[Role]

  implicit val encodePatch: Encoder[Patch] = deriveEncoder[Patch]

  implicit val decodePatch: Decoder[Patch] = new Decoder[Patch] {
    def maybeListAt(name: String)(c: HCursor): Decoder.Result[List[Role]] = {
      val field = c.downField(name)
      if (!field.succeeded)
        Right(List.empty)
      else
        field.as[List[Role]]
    }

    final def apply(c: HCursor): Decoder.Result[Patch] =
      for {
        add <- maybeListAt("add")(c)
        remove <- maybeListAt("remove")(c)
      } yield (Patch(add, remove))
  }

  def wrappedStringEncoder[A](extract: A => String) = new Encoder[A] {
    def apply(a: A): Json = Json.fromString(extract(a))
  }
  def wrappedStringDecoder[A](insert: String => A) = new Decoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] =
      c.as[String].map(insert)
  }
}
