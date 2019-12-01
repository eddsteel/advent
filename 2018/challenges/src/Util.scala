package com.eddsteel.advent.y2018

import _root_.java.io._
import _root_.scala.collection.JavaConverters._
import _root_.cats.effect._

object Util {
  def readInput(string: String): IO[List[String]] =
    Resource.fromAutoCloseable(IO {
      new BufferedReader(new InputStreamReader(getClass().getResourceAsStream(s"/input/$string")))
    }).use { reader =>
      IO(reader.lines().iterator().asScala.toList)
    }

  def readInput(day: Int): IO[List[String]] =
    readInput(day.toString)
}
