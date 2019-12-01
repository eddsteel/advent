package com.eddsteel.advent.y2018

import _root_.cats.Monoid
import _root_.cats.implicits._
import _root_.cats.effect._

object Day01 {
  case class ParseError(input: String)
  
  object Number {
    def unapply(string: String): Option[Int] =
      Either.catchOnly[NumberFormatException](string.toInt).toOption
  }

  case class Change(change: Int, visited: Set[Int] = Set(0), doubles: List[Int] = List.empty)

  object Change {
    val s1Monoid: Monoid[Change] = new Monoid[Change]() {
      override val empty = Change(0)
      override def combine(a: Change, b: Change) = Change(a.change + b.change)
    }

    val s2Monoid: Monoid[Change] = new Monoid[Change]() {
      override val empty = Change(0)
      override def combine(a: Change, b: Change) = {
        val current = a.change + b.change
        val result = 
          if (a.visited.contains(current)) a.copy(change = current, doubles = a.doubles :+ current)
          else a.copy(change = current, visited = a.visited + current)
        result
      }
    }

    def parse(s: String): Either[ParseError, Change] = s.value.splitAt(1) match {
      case ("+", Number(num)) =>
        Right(Change(num))

      case ("-", Number(num)) =>
        Right(Change(-num))

      case _ =>
        Left(ParseError(s.value))
    }
  }

  def runChallenge(combiner: Monoid[Change])(input: List[String]): Change = {
    input.map(Change.parse).collect {
      case Right(success) => success
    }.combineAll(combiner)
  }

  def combine[A](as: List[IO[List[A]]]): IO[List[A]] = as.foldLeft(IO(List.empty[A])) {
    (next: IO[List[A]], acc: IO[List[A]]) =>
       (next, acc).mapN { _ ++ _ }
  }

  def run: IO[Unit] = {
    val star1 = Util.readInput(1).map(runChallenge(Change.s1Monoid))
    val star2 = combine(List.fill(500)(Util.readInput(1))).map(runChallenge(Change.s2Monoid))

    (star1, star2).mapN { (s1, s2) =>
      println(s"Star 1: ${s1.change}")
      println(s"Star 2: ${s2.doubles.head}")
    }
  }
}


