package com.david.mixer

import scala.annotation.tailrec

class Mixer(val permutations: List[Int], val rightShifts: List[Int]) {

  import Domain._

  val rightShiftsWithIndex = rightShifts.zipWithIndex.map { case (v, i) => (i, v) }.toMap
  //For an input (3, 2, 0, 1) it would be List((0,3),(1,2),(2, 0),(3,1))
  val expectedBits = permutations.zipWithIndex.zip(rightShifts).map { case ((a, b), rightShift) => Bit(a, b, rightShift) }.toSet

  val expectedBitsMap = expectedBits.map(b => (b.value, b)).toMap

  @tailrec
  private def findDefinitions(current: List[DefinitionSnapshot], round: Int, rounds: Int): Option[DefinitionSnapshot] = {
    if (rounds == round) {
      current.filter(_.snapshot.toSet == expectedBits).headOption
    } else {
      val snapshots = current.flatMap(a => a.concatenateStep(rightShiftsWithIndex, expectedBitsMap, rounds - round - 1))
      findDefinitions(snapshots, round + 1, rounds)
    }
  }

  def findDefinitions(maxRounds: Int = 10, rounds: Int = 2): Option[Definition] = {
    val initial = 0 to permutations.size - 1
    // List[(Index, Bit) For 4Bits input would be List((0,0),(1,1),(2,2),(3,3)
    val initialBits = initial.map { case a => Bit(a, a) }
    //In every iteration of the fold we will add more permutations. In the list of perputations there is associated a snapshot of the Bits
    val initialValue = List(DefinitionSnapshot(List.empty[List[Char]], initialBits.toList))

    val output = findDefinitions(initialValue, 0, rounds)
    //If no solution has been found with the number of rounds (M) then retry to find a solution with another level
    output match {
      case None if (rounds <= maxRounds) => findDefinitions(maxRounds, rounds + 1)
      case Some(a) => Some(output.get.steps)
      case _ => None
    }
  }

}
