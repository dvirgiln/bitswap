package com.david.mixer

object Mixer {
  import Domain._
  def   combinations(permutations: List[Int], rounds: Int): List[Definition] = {
    val initial = 0 to permutations.size - 1
    // List[(Index, Bit) For 4Bits input would be List((0,0),(1,1),(2,2),(3,3)
    val initialBits = initial.map { case a => Bit(a, a) }

    //For an input (3, 2, 0, 1) it would be List((0,3),(1,2),(2, 0),(3,1))
    val expectedBits = permutations.zipWithIndex.map { case (a, b) => Bit(a, b) }.toSet

    //In every iteration of the fold we will add more permutations. In the list of perputations there is associated a snapshot of the Bits
    val initialValue = List(new DefinitionSnapshot(List.empty[List[Char]], initialBits.toList))
    //In every round we iterate over the existing accumulated definition steps and add the new steps
    val combinations = (0 to rounds - 1).foldLeft(initialValue) { case (definitions, round) => definitions.flatMap(a => a.concatenateStep) }
    //return the ones that the final snapshot is equals to the expected definition snapshot. It is being returned all the definition steps
    combinations.filter(_.snapshot.toSet == expectedBits).map(_.steps)
  }

}
