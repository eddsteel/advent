package com.eddsteel.advent.twentyone

import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.Dispatchers

fun main(): Unit {
    runBlocking(Dispatchers.IO) {
        println("star 1: ${Day02.star1()}")
        println("star 2: ${Day02.star2()}")
    }
}
