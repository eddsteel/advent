package com.eddsteel.advent.twentyone

import kotlin.sequences.generateSequence
import kotlin.sequences.asSequence
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.fold
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flowOf
import kotlinx.coroutines.flow.map

data class Counter(val zeros: Int = 0, val ones: Int = 0) {
    fun consume(input: Char) = when (input) {
        '0' -> copy(zeros = this.zeros + 1)
        '1' -> copy(ones = this.ones + 1)
        else -> throw RuntimeException("Bad input")
    }

    val maxChar
        get() = if (ones >= zeros) '1' else '0'

    val minChar
        get() = if (zeros > ones) '1' else '0'
}

fun counters(): Sequence<Counter> = generateSequence { Counter() }

data class Bin(val input: String) {
    init {
        require(input.all { c -> c == '0' || c == '1'})
    }

    constructor(chars: List<Char>): this(String(chars.toCharArray()))

    val decimal: Int
        get() = input.toInt(2)

    fun nth(i: Int) = input[i]

}

fun binOf(input: String) = Bin(input)

object Day03 {

    fun input(): Flow<Bin> = inputFlow("day03").map(::binOf)

    suspend fun count(bins: Flow<Bin>): List<Counter> =
        // use a sequence because we don't know the width
        bins.fold(counters()) { counters, bin ->
            bin.input.asSequence().zip(counters).toList().map { (char, counter) ->
                counter.consume(char)
            }.asSequence() // iter 1 it's an unbounded sequence, the rest it's fixed.
        }.toList()

    suspend fun example() {
        val input = flowOf(
            "00100", "11110", "10110", "10111", "10101", "01111",
            "00111", "11100", "10000", "11001", "00010", "01010").map(::binOf)

        println("example 1: ${star1{ input }} should be 198")
        println("example 2: ${star2{ input }} should be 230")
    }

    suspend fun star1(input: () -> Flow<Bin> = ::input): Int {
        val counts = count(input())
        val gamma = Bin(counts.map(Counter::maxChar)).decimal
        val epsilon = Bin(counts.map(Counter::minChar)).decimal

        return gamma * epsilon
    }

    suspend fun filterForRating(input: Flow<Bin>, determinant: (Counter) -> Char): Bin {
        var candidates = input.toList() // let's load them all given the problem.
        var pos = 0
        while (candidates.size > 1){
            val target = determinant(count(candidates.asFlow())[pos])
            candidates = candidates.filter { b -> b.nth(pos) == target}
            pos = pos + 1
        }

        return candidates.first()
    }

    suspend fun star2(input: () -> Flow<Bin> = ::input): Int {
        val o2GeneratorRating = filterForRating(input(), Counter::maxChar)
        val co2ScrubberRating = filterForRating(input(), Counter::minChar)

        return o2GeneratorRating.decimal * co2ScrubberRating.decimal
    }
}
