package com.eddsteel.advent17
import cmd.AdventCommand
import _root_.com.monovore.decline.CommandApp

object Main
    extends CommandApp(
      name = "advent",
      header = "interact with advent of code",
      main = AdventCommand.main
    ) {
  def cat: String = "😸"
}
u
