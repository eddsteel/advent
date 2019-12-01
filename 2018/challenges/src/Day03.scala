package com.eddsteel.advent.y2018

import _root_.cats.effect._
import _root_.cats.implicits._

object Day03 {
  type ClaimNo = Int
  case class Board(pixels: Map[(Int, Int), List[ClaimNo]], claims: List[Claim]) {
    def addClaim(claim: Claim) =
      Board(pixels.combine(claim.pixels.map(_ -> List(claim.claim)).toMap), claims :+ claim)

    def nonEmpty = pixels.toList.count(_._2.nonEmpty)
    def overlapping = pixels.toList.count(_._2.length > 1)
    def draw: List[String] = {
      val maxX = pixels.keys.map(_._1).max
      val maxY = pixels.keys.map(_._2).max

      (0.to(maxY + 1)).map { y =>
        (0.to(maxX + 1)).map { x =>
          val claims = pixels.get((x, y)).getOrElse(Nil)
          claims match {
            case Nil => '.'
            case List(claim) => claim.toString.head
            case _ => 'x'
          }
        }.toList
      }.toList.transpose.map(_.mkString)
    }

    val overlappingClaims = pixels.toList.flatMap {
      case (_, v) if v.size > 1 => v
      case _ => Nil
    }

    val onlyNonOverlappingClaim = claims.filterNot { claim =>
      overlappingClaims.contains(claim.claim)
    }
  }

  object Board {
    def empty: Board = Board(Map.empty, Nil)
  }

  // #1 @ 555,891: 18x12
  case class Claim(claim: ClaimNo, x: Int, y: Int, w: Int, h: Int) {
    def pixels =
      for {
        i <- y.until(y + h)
        j <- x.until(x + w)        
      } yield (i, j)
  }

  object Claim {
    private val regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)+x([0-9]+)".r

    def parse(s: String): Option[Claim] = s match {
      case regex(c, x, y, w, h) =>
        Some(Claim(c.toInt, x.toInt, y.toInt, w.toInt, h.toInt))
      case x =>
        println(s"miss on $x")
        None
    }
  }

  def solve(lines: List[String]) = {
    val claims = lines.flatMap(Claim.parse)
    val board = claims.foldLeft(Board.empty){
      case (acc, next) => acc.addClaim(next)
    }
    board.overlapping -> board.onlyNonOverlappingClaim
  }

  def run: IO[Unit] = for {
    lines <- Util.readInput(3)
    (s1, s2) = solve(lines)
  } yield println(s"Star 1: ${s1}\nStar 2: $s2")
}
