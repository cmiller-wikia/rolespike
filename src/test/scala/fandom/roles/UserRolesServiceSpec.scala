package fandom
package roles

import fs2.Task
import fs2.interop.cats._
import org.http4s._
import org.scalatest._
import Matchers._
import HttpMatchers._
import io.circe.literal._
import org.http4s.circe._
import Predef.ArrowAssoc
import cats.syntax.flatMap._
import webtests._

// Note that while the Role DB does not enforce any particular ordering
// of roles, this test thoroughly abuses the predictability of ordering
// in the in-memory implementation.
class UserRolesServiceSpec extends FreeSpec {
  val defaultState: List[Grant] = fixtures.defaultGrants

  def service(fixtures: List[Grant]): Request => Task[MaybeResponse] =
    UserRolesService.service(
      MemoryRoleDb,
      apitests.stateToWebOp(fixtures)
    ).apply _

  def serve(req: Task[Request])(fixtures: List[Grant]): Task[MaybeResponse] =
    req >>= service(fixtures)

  "The user roles service" - {
    "when getting the roles for a user" - {
      "should be empty if the user does not exist" in {
        serve(get("/roles/users/bob"))(List.empty) should
          respondWithConformingJson(json"""[]""")
      }

      "should return all roles if no scope filter is provided" in {
        serve(get("/roles/users/bob"))(defaultState) should
          respondWithConformingJson(json"""[
					  { "name": "staff", "scope": "global" },
						{ "name": "discussions-moderator", "scope": "wiki:831" },
					  { "name": "discussions-helper", "scope": "wiki:832" }
					]""")
      }

      "should filter by roles if scope filter is provided" in {
        serve(get("/roles/users/bob", "scope" -> "wiki:831"))(defaultState) should
          respondWithConformingJson(json"""[
						{ "name": "discussions-moderator", "scope": "wiki:831" }
					]""")
      }

      "should filter by multiuple roles if scope filter is multiple" in {
        serve(get("/roles/users/bob", "scope" -> "wiki:831", "scope" -> "global"))(defaultState) should
          respondWithConformingJson(json"""[
					  { "name": "staff", "scope": "global" },
						{ "name": "discussions-moderator", "scope": "wiki:831" }
					]""")
      }
    }

    "when deleting roles for a user" - {
      "should be fine if the user does not exist" in {
        serve(delete("/roles/users/bob"))(List.empty).
          should(respondWithStatus(Status.NoContent))
      }

      "should remove all the roles if the user exists" in {
        val svc = service(defaultState)
        (delete("/roles/users/bob") >>= svc).
          should(respondWithStatus(Status.NoContent))
        (get("/roles/users/bob") >>= svc) should
          respondWithConformingJson(json"""[]""")
      }

      "should limit role removal by a single scope" in {
        val svc = service(defaultState)
        (delete("/roles/users/bob", "scope" -> "wiki:831") >>= svc).
          should(respondWithStatus(Status.NoContent))
        (get("/roles/users/bob") >>= svc) should
          respondWithConformingJson(json"""[
					{ "name": "staff", "scope": "global" },
					{ "name": "discussions-helper", "scope": "wiki:832" }
				]""")
      }

      "should limit role removal by a multiple scopes" in {
        val svc = service(defaultState)
        (delete(
          "/roles/users/bob",
          "scope" -> "wiki:831",
          "scope" -> "global"
        ) >>= svc).should(respondWithStatus(Status.NoContent))

        (get("/roles/users/bob") >>= svc) should
          respondWithConformingJson(json"""[
					{ "name": "discussions-helper", "scope": "wiki:832" }
				]""")
      }
    }

    "when updating roles for a user" - {
      "should reject badly formed document" in {
        serve(post(
          "/roles/users/bob",
          json"""{ "add" : [ { "foo": "bar" } ] }"""
        ))(defaultState) should respondWithStatus(Status.UnprocessableEntity)
      }

      "should reject document with neither adds nor removes" in {
        serve(post(
          "/roles/users/bob",
          json"""{ }"""
        ))(defaultState) should respondWithStatus(Status.UnprocessableEntity)
      }

      "should reject document with empty adds and removes" in {
        serve(post(
          "/roles/users/bob",
          json"""{ "add": [], "remove": [] }"""
        ))(defaultState) should respondWithStatus(Status.UnprocessableEntity)
      }

      "should work just adding roles" in {
        serve(post(
          "/roles/users/harold",
          json"""{
						"add": [
						   { "name": "staff", "scope": "global" },
							 { "name": "vstf", "scope": "global" }
						]
					}"""
        ))(defaultState) should respondWithConformingJson(
          json"""[
						{ "name": "discussions-helper", "scope": "wiki:831" },
						{ "name": "staff", "scope": "global" },
						{ "name": "vstf", "scope": "global" }
					]"""
        )
      }

      "should accept adds for nonexistent users" in {
        serve(post(
          "/roles/users/bob",
          json"""{
						"add": [
							{ "name": "vstf", "scope": "global" }
						]
					}"""
        ))(List.empty) should respondWithConformingJson(
          json"""[
							{ "name": "vstf", "scope": "global" }
						]"""
        )
      }

      "should accept deletes for nonexistent users" in {
        serve(post(
          "/roles/users/bob",
          json"""{
						"remove": [
							{ "name": "vstf", "scope": "global" }
						]
					}"""
        ))(List.empty) should respondWithConformingJson(
          json"""[]"""
        )
      }

      "should accept deletes for nonexistent roles" in {
        serve(post(
          "/roles/users/harold",
          json"""{
						"remove": [
							{ "name": "vstf", "scope": "global" }
						]
					}"""
        ))(defaultState) should respondWithConformingJson(
          json"""[ { "name": "discussions-helper", "scope": "wiki:831" } ]"""
        )
      }

      "should process adds then deletes" in {
        serve(post(
          "/roles/users/harold",
          json"""{
						"add": [
						   { "name": "staff", "scope": "global" },
							 { "name": "vstf", "scope": "global" }
						],
						"remove": [
						   { "name": "staff", "scope": "global" },
							 { "name": "discussions-helper", "scope": "wiki:831" }
						]
					}"""
        ))(defaultState) should respondWithConformingJson(
          json"""[
						{ "name": "vstf", "scope": "global" }
					]"""
        )
      }
    }
  }
}
