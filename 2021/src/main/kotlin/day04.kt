package com.eddsteel.advent.twentyone

import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.last
import kotlinx.coroutines.flow.mapNotNull

data class Board(val rows: List<List<Int>>) {
    val cols
        get() = rows.transpose()

    val render
        get() = rows.joinToString("\n") { r -> r.joinToString(" ") }

    fun isBingo(drawing: List<Int>): Boolean =
        rows.any(drawing::containsAll) || cols.any(drawing::containsAll)

    fun score(drawing: List<Int>): Int =
        rows.flatten().filterNot(drawing::contains).sum() * drawing.last()
}

data class Drawing(val draw: List<Int>)
data class Game(val boards: List<Board>, val drawing: Drawing) {
    val render
        get() = "${drawing.draw.joinToString(",", "[", "]")}\n\n${boards.joinToString("\n\n"){ b -> b.render }}"
}

data class GameBuilder(
    val drawing: Drawing? = null,
    val boards: List<Board> = emptyList(),
    val boardInProgress: List<List<Int>> = emptyList()) {

    private fun rowOf(s: String): List<Int> = s.trim().split(Regex(" +")).filter { it.isNotEmpty() }.map { it.trim().toInt() }.toList()

    fun accept(s: String): GameBuilder =
        when {
            s.contains(',') && drawing != null -> badInput()
            s.contains(',') -> copy(drawing = Drawing(s.trim().split(',').map { it.trim().toInt() }))
            s.trim() == "" && boardInProgress.isEmpty() -> this // skip first divider.
            s.trim() == "" -> copy(boards = boards.plusElement(Board(boardInProgress)), boardInProgress = emptyList())
            else -> copy(boardInProgress = boardInProgress.plusElement(rowOf(s)))
        }

    fun build(): Game =
        if (drawing != null) Game(boards.plusElement(Board(boardInProgress)), drawing)
        else throw badInput()
}

fun gameOf(lines: List<String>): Game = lines.fold(GameBuilder()) { g, s -> g.accept(s) }.build()

object Day04 {
    fun input(): Game = gameOf(input("day04"))

    suspend fun example() {
        val game = gameOf(listOf(
            "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
            "              ",
            "22 13 17 11  0", " 8  2 23  4 24", "21  9 14 16  7", " 6 10  3 18  5", " 1 12 20 15 19",
            "              ",
            " 3 15  0  2 22", " 9 18 13 17  5", "19  8  7 25 23", "20 11 10 24  4", "14 21 16 12  6",
            "              ",
            "14 21 17 24  4", "10 16 15  9 19", "18  8 23 26 20", "22 11 13  6  5", "2  0 12  3  7 "
        ))

        println("Example 1: ${star1 { game }}")
        println("Example 2: ${star2 { game }}")
    }

    suspend fun gameplay(game: Game): Flow<Int> {
        var contenders = game.boards
        return game.drawing.draw.subLists().asFlow().mapNotNull { draw ->
            val roundWinner = contenders.firstOrNull { board -> board.isBingo(draw) }
            val score = roundWinner?.score(draw)
            contenders = contenders.filterNot { it.isBingo(draw) }
            score
        }
    }

    suspend fun star1(game: () -> Game = ::input): Int = gameplay(game()).first()
    suspend fun star2(game: () -> Game = ::input): Int = gameplay(game()).last()
}
