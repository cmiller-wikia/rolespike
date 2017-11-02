package fandom
package healthcheck

trait HealthChecker[F[_]] {
  def isHealthy: F[Boolean]
}
