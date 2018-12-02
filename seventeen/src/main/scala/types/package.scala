package com.eddsteel.advent17
import _root_.eu.timepit.refined._
import _root_.eu.timepit.refined.api._
import _root_.eu.timepit.refined.boolean._
import _root_.eu.timepit.refined.numeric._

package object types {
  type Year = Refined[Int, Y]
  type Day = Refined[Int, D]

  type Y = Not[Less[W.`2015`.T]] And Not[Greater[W.`2017`.T]]
  type D = Positive And Not[Greater[W.`25`.T]]
}
