package com.eddsteel.advent.twentyone

import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.Dispatchers

fun main(): Unit {
    runBlocking(Dispatchers.IO) {
        Day04.example()
        println("Star 1: ${Day04.star1()}")
        println("Star 2: ${Day04.star2()}")
    }
}
