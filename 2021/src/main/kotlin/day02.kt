package com.eddsteel.advent.twentyone

import kotlinx.coroutines.flow.fold
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.count
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.flowOf
import kotlinx.coroutines.flow.map

data class Position(val horizontal: Int, val depth: Int, val aim: Int = 0)

sealed interface Command
data class Forward(val x: Int): Command
data class Down(val x: Int): Command
data class Up(val x: Int): Command

fun commandOf(s: String): Command {
    val matches: List<Pair<String, (MatchResult) -> Command>> = listOf(
        "forward ([0-9]+)" to {x -> Forward(x.groupValues[1].toInt())},
        "down ([0-9]+)" to {x -> Down(x.groupValues[1].toInt())},
        "up ([0-9]+)" to {x -> Up(x.groupValues[1].toInt())}
    )
    matches.forEach { (re, f) ->
        val result = Regex(re).matchEntire(s)
        if (result != null) return(f(result))
    }

    throw RuntimeException("Invalid input")
}

object Day02 {
    private fun input(): Flow<Command> = inputFlow("day02").map { commandOf(it) }

    suspend fun example() {
        val exampleFlow = {
            flowOf(
                "forward 5", "down 5", "forward 8",
                "up 3", "down 8", "forward 2"
            ).map { commandOf(it) }
        }
        println("example 1: ${star1(exampleFlow)} should be 150")
        println("example 2: ${star2(exampleFlow)} should be 900")
    }

    suspend fun star1(input: () -> Flow<Command> = ::input): Int {
        val end = input().fold(Position(0, 0)) { pos, comm ->
            when (comm) {
                is Forward -> pos.copy(horizontal = pos.horizontal + comm.x)
                is Down -> pos.copy(depth = pos.depth + comm.x)
                is Up -> pos.copy(depth = pos.depth - comm.x)
            }
        }

        return end.horizontal * end.depth
    }

    suspend fun star2(input: () -> Flow<Command> = ::input): Int {
        val end = input().fold(Position(0, 0)) { pos, comm ->
        when (comm) {
                is Forward -> pos.copy(horizontal = pos.horizontal + comm.x,
                                       depth = pos.depth + pos.aim * comm.x)
                is Down -> pos.copy(aim = pos.aim + comm.x)
                is Up -> pos.copy(aim = pos.aim - comm.x)
            }
        }

        return end.horizontal * end.depth
    }
}
