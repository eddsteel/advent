package com.eddsteel.advent
import _root_.cats.derived.semi
import _root_.cats.Show
import ciris.ConfigErrors

object AdventError {
  implicit val showConfigErrors: Show[ConfigErrors] = Show.fromToString[ConfigErrors]
  implicit val showAdventError: Show[AdventError] = semi.show[AdventError]
}

sealed trait AdventError
final case class BadConfig(issues: ConfigErrors) extends AdventError
