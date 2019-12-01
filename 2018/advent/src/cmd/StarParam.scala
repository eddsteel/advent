package com.eddsteel.advent
package cmd
import types.Star
import types.Star._
import _root_.cats.Eq
import _root_.cats.implicits._
import _root_.com.monovore.decline.Argument

object StarParam {

  implicit val arg: Argument[Star] = new Argument[Star]() {

    def defaultMetavar: String = "star"

    def read(string: String) =
      (string match {
        case "1" => Star1.valid[String]
        case "2" => Star2.valid[String]
        case x   => s"$x is not a valid star. Use '1' or '2'".invalid[Star]
      }).toValidatedNel
  }
}
