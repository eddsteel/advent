package com.eddsteel.advent17
package cmd
import types._
import aoc._
import _root_.cats.data.EitherT
import _root_.cats.effect.IO
import _root_.cats.implicits._
import _root_.com.monovore.decline._
import _root_.com.monovore.decline.refined._

@SuppressWarnings(Array("org.wartremover.warts.Nothing"))
object AdventCommand {
  val main: Opts[Unit] = {
    val day = Opts.argument[Day]("day")
    val input = Opts.argument[String]("input")

    def run(f: AdventOfCode => IO[AdventErrorOr[Unit]]): Unit =
      (for {
        config <- EitherT(AdventOfCodeConfig.load)
        aoc = AdventOfCode(config)
        _ <- EitherT(f(aoc))
      } yield ()).value.flatMap {
        case Left(issue) => IO(sys.error(issue.show))
        case Right(v)    => IO(v)
      }.unsafeRunSync

    val getDescriptionCmd = Opts.subcommand(
      Command(
        name = "description",
        header = "get a challenge's description"
      ) {
        day.map { d =>
          run(_.getDescription(d))
        }
      })

    val getInputCmd = Opts.subcommand(
      Command(
        name = "input",
        header = "get a challenge's input"
      ) {
        day.map { d =>
          run(_.getInput(d))
        }
      })

    val postSolution = Opts.subcommand(
      Command(
        name = "submit",
        header = "submit a challenge's solution"
      ) {
        (day, input).mapN { case (d, i) => run(_.postSolution(d, i)) }
      })

    getDescriptionCmd.orElse(getInputCmd).orElse(postSolution)
  }
}
