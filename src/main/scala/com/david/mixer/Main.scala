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

  /*val permutations = readPermutations(false)
  val rightShifts = readRightShifts(false)
  println(Mixer.findDefinitions(permutations, rightShifts))*/

  val v= Mixer.findDefinitions(List(0,3,1,2,5,7,4,8,6,9), List(1,3,3,1,2,2,3,1,1,1),3,3)
  //val v2= Mixer.findDefinitions(List(0,1,7,2,5,3,11,13,4,6,8,14,9,12,10), List(10,15,14,13,15,12,15,11,13,13,13,11,10,8,9),6,6)
}
