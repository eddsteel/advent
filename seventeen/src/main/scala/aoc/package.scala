package com.eddsteel.advent17

package object aoc {
  type AdventErrorOr[A] = Either[AdventError, A]
}
