package com.eddsteel.advent17
package aoc
import cats.derive
import cats.Show
import ciris.ConfigErrors

object AdventError {
  implicit val showConfigErrors: Show[ConfigErrors] = Show.fromToString[ConfigErrors]
  implicit val showAdventError: Show[AdventError] = derive.show[AdventError]
}

sealed trait AdventError
final case class BadConfig(issues: ConfigErrors) extends AdventError
