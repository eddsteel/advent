package com.eddsteel.advent
import types._
import _root_.cats.effect.IO
import _root_.cats.syntax.either._
import _root_.ciris._
import _root_.ciris.refined._
import _root_.com.softwaremill.sttp._
import _root_.eu.timepit.refined._
import _root_.eu.timepit.refined.types.string.NonEmptyString
import _root_.com.softwaremill.sttp.Uri
import _root_.java.time.{Year => JYear}

final case class AdventOfCodeConfig(
  year: Year,
  baseUrl: Uri,
  cookie: eu.timepit.refined.types.string.NonEmptyString
)

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object AdventOfCodeConfig {
  def load: IO[AdventErrorOr[AdventOfCodeConfig]] = IO {
    loadConfig(
      env[NonEmptyString]("COOKIE")
    )(
      AdventOfCodeConfig(refineMV[Y](2018), uri"http://adventofcode.com", _)
    ).leftMap(BadConfig.apply(_))
  }
}
