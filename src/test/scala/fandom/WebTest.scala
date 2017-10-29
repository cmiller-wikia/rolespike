package fandom

import org.http4s._
import fs2.Task
import fs2.interop.cats._
import cats.syntax.applicative._

trait WebTestOps {
  def get(path: String, params: (String, String)*): Task[Request] =
    Request(
      uri = Uri(
        path = path,
        query = Query.fromPairs(params: _*)
      )
    ).pure[Task]

  def delete(path: String, params: (String, String)*): Task[Request] =
    Request(
      method = Method.DELETE,
      uri = Uri(path = path, query = Query.fromPairs(params: _*))
    ).pure[Task]

  def post[A: EntityEncoder](path: String, body: A): Task[Request] =
    Request(
      method = Method.POST,
      uri = Uri(path = path)
    ).withBody(body)
}

object webtests extends WebTestOps
