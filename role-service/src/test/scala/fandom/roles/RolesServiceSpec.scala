package fandom
package roles

import fs2.Task
import fs2.interop.cats._
import org.http4s._
import org.scalatest._
import Matchers._
import HttpMatchers._
import io.circe.literal._
import Predef.ArrowAssoc
import cats.syntax.flatMap._
import webtests._

// Note that while the Role DB does not enforce any particular ordering
// of roles, this test thoroughly abuses the predictability of ordering
// in the in-memory implementation.
class RolesServiceSpec extends FreeSpec {
  val defaultState: List[Grant] = fixtures.defaultGrants

  def service(fixtures: List[Grant]): Request ⇒ Task[MaybeResponse] =
    RolesService.service(
      MemoryRoleDb,
      apitests.stateToWebOp(fixtures)
    ).apply _

  def serve(req: Task[Request])(fixtures: List[Grant]): Task[MaybeResponse] =
    req >>= service(fixtures)

  def bulkQuery(makeRequest: (String, (String, String)*) ⇒ Task[Request]): Unit = {
    "Should return an error if no users are queried" in {
      serve(makeRequest("/roles", "scope" -> "wiki:831"))(defaultState) should
        respondWithStatus(Status.BadRequest)
    }

    "Should return all roles if a user is queried with no scope filter" in {
      serve(makeRequest(
        "/roles",
        "userId" -> "harold"
      ))(defaultState) should respondWithConformingJson(
        json"""{
         "harold": [
           { "name": "discussions-helper", "scope": "wiki:831" }
         ]
        }"""
      )
    }

    "Should still return a key if a user is queried but their roles are filtered" in {
      serve(makeRequest(
        "/roles",
        "userId" -> "harold",
        "scope" -> "global"
      ))(defaultState) should respondWithConformingJson(
        json"""{
          "harold": []
        }"""
      )
    }

    "Should filter multiple users/roles some not existing" in {
      serve(makeRequest(
        "/roles",
        "userId" -> "harold",
        "userId" -> "carol",
        "userId" -> "bob",
        "scope" -> "global",
        "scope" -> "wiki:831",
        "scope" -> "wiki:432"
      ))(defaultState) should respondWithConformingJson(
        json"""{
          "harold": [
            { "name": "discussions-helper", "scope": "wiki:831" }
          ],
          "carol": [],
          "bob": [
            { "name": "staff", "scope": "global" },
            { "name": "discussions-moderator", "scope": "wiki:831" }
          ]
        }"""
      )
    }
  }

  "The roles service" - {
    "When querying for bulk role data with GET" - {
      behave like bulkQuery(
        (path, params) ⇒ get(path, params: _*)
      )
    }

    "When querying for bulk role data with POST" - {
      behave like bulkQuery(
        (path, params) ⇒ post(path, UrlForm(params: _*))
      )
    }
  }
}
