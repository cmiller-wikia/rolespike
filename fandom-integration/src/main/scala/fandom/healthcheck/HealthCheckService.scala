package fandom
package healthcheck

import cats._
import cats.instances.unit._
import fs2.interop.cats._
import org.http4s._
import org.http4s.dsl._
import web._
import transforms._

/**
 * Trivial liveness service, with the option to provide an externally configured checker.
 */
trait HealthCheckService[F[_]] {
  val HC: HealthChecker[F]
  val T: F ~> WebOp
  implicit val M: Monad[F]

  val service = HttpService {
    case req @ GET -> Root / "healthcheck" ⇒ run(areWeHealthy)(req)
  }

  def areWeHealthy: WebService =
    for {
      healthy ← T(HC.isHealthy)
      response ← if (healthy) liftTask(Ok()) else liftTask(ServiceUnavailable())
    } yield (response)
}

object HealthCheckService {
  def service[F[_]](HCC: HealthChecker[F], TT: F ~> WebOp)(implicit MM: Monad[F]): HttpService =
    new HealthCheckService[F] {
      val HC = HCC
      val T = TT
      val M = MM
    }.service

  val alwaysHealthy =
    service(
      new HealthChecker[Id] {
        def isHealthy = true
      },
      idToWebOp
    )
}
