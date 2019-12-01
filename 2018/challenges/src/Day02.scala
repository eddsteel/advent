package com.eddsteel.advent.y2018

import _root_.cats.Apply
import _root_.cats.implicits._
import _root_.cats.effect._

object Day02 {
  def hasNSameChars(n: Int)(s: String): Boolean = 
    s.toList.groupBy(identity).map {
      case (_, vs) => vs.length
    }.exists(_ == n)

  def differsByNChars(n: Int)(s1: String, s2: String): Boolean = {
    val m = s1.toList.zip(s2.toList).count{ case (x, y) => x != y}
    if (n == m) println(s"$s1 <> $s2: $m chars different")
    n == m
  }
    
  def commonChars(s1: String, s2: String) =
    s1.zip(s2).filter{case (x, y) => x == y}.map(_._1).mkString

  def star1(lines: List[String]): String = {
    val doubles = lines.count(hasNSameChars(2))
    val triples = lines.count(hasNSameChars(3))
    val checksum = doubles * triples
    checksum.toString
  }

  def star2(lines: List[String]): String = {
    Apply[List].product(lines, lines).collectFirst {
      case (s1, s2) if differsByNChars(1)(s1, s2) =>
        commonChars(s1, s2)      
    }.getOrElse("<fail>")
  }

  def run: IO[Unit] = for {
    lines <- Util.readInput(2)
    s1 = star1(lines)
    s2 = star2(lines)
  } yield println(s"Star 1: ${s1}\nStar 2: $s2")
}
