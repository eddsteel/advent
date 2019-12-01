package com.eddsteel.advent
import types._
import _root_.cats.implicits._
import _root_.cats.effect._
import _root_.eu.timepit.refined.types.string.NonEmptyString
import _root_.com.softwaremill.sttp._
import _root_.scala.concurrent.ExecutionContext.Implicits.global

class RemoteAdventOfCode(val year: Year, base: Uri, session: NonEmptyString)
    extends AdventOfCode {
  import quick._ // sync backend

  def get(path: String): IO[String] = {
    val request = sttp.header("Cookie", s"session=$session").get(base.path(path))
    for {
      resp <- IO(request.send().body)
      succ <- IO.fromEither(resp.leftMap(s => new RuntimeException(s"Unexpected response: $s")))
    } yield succ
  }

  def post(path: String, params: (String, String)*): IO[Unit] =
    // not working yet.
    IO(println(base.path(path)))
  /*    val request =
      POST(
        base.withPath(path),
        UrlForm(params: _*),
        headers = Seq(()
          Header("Cookie", cookie.toString),
          Header("DNT", "1"),
          Header("Upgrade-Insecure-Requests", "1")): _*)

    for {
      resp <- httpClient.expect[String](request)
      _ <- IO(println(resp))
    } yield ()*/
}
