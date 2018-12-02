package com.eddsteel.advent17
package aoc
import types._
import _root_.cats.effect.IO
import _root_.cats.syntax.either._
import _root_.ciris._
import _root_.ciris.refined._
import _root_.eu.timepit.refined._
import _root_.eu.timepit.refined.types.string.NonEmptyString
import _root_.java.net.URI

final case class AdventOfCodeConfig(
  year: Year,
  baseUrl: URI,
  cookie: NonEmptyString
)

object AdventOfCodeConfig {
  def load: IO[AdventErrorOr[AdventOfCodeConfig]] = IO {
    loadConfig(
      env[NonEmptyString]("COOKIE")
    )(
      AdventOfCodeConfig(refineMV[Y](2017), new URI("http://adventofcode.com"), _)
    ).leftMap(BadConfig.apply)
  }
}
