package fandom
package healthcheck

import fs2.Task
import fs2.interop.cats._
import org.http4s._
import org.scalatest._
import Matchers._
import HttpMatchers._
import cats._
import cats.syntax.flatMap._
import webtests._
import transforms._

class HealthCheckServiceSpec extends FreeSpec {
  def staticHealthChecker(health: Boolean): HealthChecker[Id] = new HealthChecker[Id] {
    def isHealthy = health
  }

  def service(health: Boolean): Request ⇒ Task[MaybeResponse] =
    HealthCheckService.service(
      staticHealthChecker(health),
      idToWebOp
    ).apply _

  def serve(req: Task[Request])(health: Boolean): Task[MaybeResponse] =
    req >>= service(health)

  "The health check service" - {
    "should succeed when healthy" in {
      serve(get("/healthcheck"))(true) should respondWithStatus(Status.Ok)
    }
    "should fail when unhealthy" in {
      serve(get("healthcheck"))(false) should respondWithStatus(Status.ServiceUnavailable)
    }
  }
}
