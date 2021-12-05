package com.eddsteel.advent.twentyone

import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.Dispatchers

fun main(): Unit {
    runBlocking(Dispatchers.IO) {
        Day03.example()
        println(Day03.star1())
        println(Day03.star2())
    }
}
