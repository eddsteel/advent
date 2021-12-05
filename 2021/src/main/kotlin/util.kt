package com.eddsteel.advent.twentyone

import java.nio.file.Files
import java.nio.file.Paths
import kotlin.streams.asSequence
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.collect

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

fun inputFlow(name: String): Flow<String> =
    Files
        .lines(Paths.get("inputs", name))
        .asSequence()
        .asFlow()
