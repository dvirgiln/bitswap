package com.david.mixer

import scala.io.StdIn
import scala.util.Try

object Main extends App {
  def readPermutations(error: Boolean): List[Int] = {
    if (error) {
      println("\tValue introduced not correct. Please introduce a correct value for the permutations.")
    }
    println("Permutations(comma separated list of integers like 2,0,1): ")
    Try(StdIn.readLine.split(",").map(_.toInt).toList).getOrElse(readPermutations(true))
  }

  def readRightShifts(error: Boolean): List[Int] = {
    if (error) {
      println("\tValue introduced not correct. Please introduce a correct value for the right shifts.")
    }
    println("Right Shifts(comma separated list of integers like 2,0,1): ")
    Try(StdIn.readLine.split(",").map(_.toInt).toList).getOrElse(readRightShifts(true))
  }

  val permutations = readPermutations(false)
  val rightShifts = readRightShifts(false)
  println(Mixer.findDefinitions(permutations, rightShifts))
}
