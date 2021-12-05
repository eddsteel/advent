package com.eddsteel.advent.twentyone

import kotlin.streams.asSequence
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.Dispatchers

fun main(): Unit {
    runBlocking(Dispatchers.IO) {
        Day01.example()
        println(Day01.star1())
        println(Day01.star2())
    }
}
