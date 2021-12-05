package com.eddsteel.advent.twentyone

import com.eddsteel.advent.twentyone.windowed
import com.eddsteel.advent.twentyone.inputFlow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.count
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.flowOf
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.zip

object Day01 {
    private fun input(): Flow<Int> = inputFlow("day01").map { it.toInt() }

    private val ascending = fun(l: List<Int>): Boolean {
        require(l.size == 2)
        return l[0] < l[1]
    }

    suspend fun example() {
        val example = { flowOf(199, 200, 208, 210, 200, 207, 240, 269, 260, 263) }
        val i = star1(example)
        println("example 1: $i should be 7")
        val j = star2(example)
        println("example 2: $j should be 5")
    }

    suspend fun star1(input: () -> Flow<Int> = ::input): Int =
        input()
        .windowed(2)
        .filter(ascending)
        .count()

    suspend fun star2(input: () -> Flow<Int> = ::input): Int =
        input()
        .windowed(3)
        .map { it.sum() }
        .windowed(2)
        .filter(ascending)
        .count()
}
