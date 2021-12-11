package com.eddsteel.advent.twentyone

import java.nio.file.Files
import java.nio.file.Paths
import kotlin.streams.asSequence
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.take
import kotlinx.coroutines.flow.toList

fun badInput(): Nothing = throw RuntimeException("Bad Input!")

suspend fun <R> Flow<R>.windowed(n: Int): Flow<List<R>> {
    val flw: Flow<R> = this
    return flow() {
        val buffer = mutableListOf<R>()
        flw.collect { value ->
            buffer.add(value)
            if (buffer.size >= n) {
                emit(buffer.toList())
                buffer.removeAt(0)
            }
        }
    }
}

fun <T> List<List<T>>.transpose(): List<List<T>> {
    val cols = first().indices.map { mutableListOf<T>() }
    this.forEach { row ->
        cols.zip(row).forEach {
            (col, element) -> col.add(element)
        }
    }
    return cols.map { it.toList() }
}

fun <T> List<T>.subLists(): Sequence<List<T>> = sequence {
    yieldAll((0..size).map { i -> subList(0, i)})
}

fun input(name: String): List<String> =
    Files.lines(Paths.get("inputs", name)).toList()

fun inputFlow(name: String): Flow<String> =
    Files.lines(Paths.get("inputs", name)).asSequence().asFlow()
