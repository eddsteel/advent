package com.eddsteel.advent
import _root_.cats.effect.IO
import _root_.java.nio.file.{Files, Path, Paths}
import _root_.scala.collection.JavaConverters._

class PassThroughAdventOfCode(aoc: AdventOfCode) extends AdventOfCode {
  import PassThroughAdventOfCode._

  def year = aoc.year

  def get(path: String): IO[String] = {
    val filepath = Paths.get(s"local${path}.txt")

    exists(filepath).flatMap { exists =>
      if (exists)
        read(filepath)
      else
        for {
          content <- aoc.get(path)
          _ <- write(filepath, content)
        } yield content
    }
  }

  def post(path: String, params: (String, String)*): IO[Unit] =
    aoc.post(path, params: _*)
}

object PassThroughAdventOfCode {
  def exists(path: Path): IO[Boolean] = IO(
    Files.exists(path)
  )

  def read(path: Path): IO[String] = IO(
    Files.readAllLines(path).asScala.mkString("\n")
  )

  def write(path: Path, content: String): IO[Unit] =
    for {
      _ <- IO(Files.createDirectories(path.getParent()))
      _ <- IO(Files.write(path, content.getBytes))
    } yield ()
}
