package fandom

import fs2.Task
import fs2.interop.cats._
import cats._
import cats.syntax.applicative._
import cats.instances.unit._
import org.http4s._
import cats.arrow.FunctionK

trait WebOps {
  type WebOp[A] = ProgramT[Task, Request, Unit, Response, A]
  type WebService = WebOp[Response]

  implicit val WM = ProgramT.monad[Task, Request, Unit, Response]
  implicit val WL = ProgramT.transLift[Request, Unit, Response]
  val taskToWebOp: (Task ~> WebOp) = FunctionK.lift(liftTask)

  def liftTask[A](task: Task[A]): WebOp[A] = WL.liftT(task)
  def sendError[A](err: Task[Response]): WebOp[A] = WM.raiseErrorM(err)
  val askRequest: WebOp[Request] = WM.ask
  def withRequest[A](f: Request ⇒ A): WebOp[A] = askRequest.map(f)

  def run(svc: WebService)(req: Request): Task[Response] =
    svc.run(req).map(_._2.merge)
}

trait BodySupport extends WebOps {
  def bodyAs[A: EntityDecoder]: WebOp[A] =
    for {
      request ← askRequest
      decodeResult ← liftTask(request.attemptAs[A].value)
      decoded ← decodeResult.fold(
        err ⇒ sendError(err.toHttpResponse(request.httpVersion)),
        succ ⇒ succ.pure[WebOp])
    } yield (decoded)
}

trait FormSupport extends WebOps {
  def multiParam[A](
    name: String,
    mapWith: (String ⇒ A) = Predef.identity[String](_)): WebOp[List[A]] =
    withRequest(
      _.multiParams
        .get(name)
        .map(_.map(mapWith).toList)
        .getOrElse(List.empty))
}

object web extends WebOps with FormSupport with BodySupport
