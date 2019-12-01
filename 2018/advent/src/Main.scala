package com.eddsteel.advent
import cmd.AdventCommand
import _root_.com.monovore.decline.CommandApp

object Main extends CommandApp(
  name   = "advent",
  header = "interact with advent of code",
  main   = AdventCommand.main
)
