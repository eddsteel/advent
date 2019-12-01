package com.eddsteel.advent
import _root_.cats.{Eq, Show}
import _root_.cats.derived
import _root_.eu.timepit.refined._
import _root_.eu.timepit.refined.api._
import _root_.eu.timepit.refined.boolean._
import _root_.eu.timepit.refined.numeric._

package object types {
  type Year = Refined[Int, Y]
  type Day = Refined[Int, D]

  sealed trait Star
  object Star {
    case object Star1 extends Star
    case object Star2 extends Star

    @SuppressWarnings(Array("org.wartremover.warts.Equals")) // this is wack
    implicit val eqStar: Eq[Star] = {
      import derived.auto.eq._
      derived.semi.eq[Star]
    }
  }

  type Y = Not[Less[W.`2015`.T]] And Not[Greater[W.`2018`.T]]
  type D = Positive And Not[Greater[W.`25`.T]]

  implicit val showYear: Show[Year] = Show.fromToString[Year]
  implicit val showDay: Show[Day] = Show.fromToString[Day]
}
