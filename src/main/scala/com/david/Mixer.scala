package com.david

object Mixer {
  import Domain._
  def combinations(permutations: List[Int],rounds: Int):List[Definition] ={
    val initial = 0 to permutations.size
    // List[(Index, Bit) For 4Bits input would be List((0,0),(1,1),(2,2),(3,3)
    val initialWithIndex= initial.zipWithIndex.map{case (a, b) => Bit(b, a)}

    //For an input (3, 2, 0, 1) it would be List((0,3),(1,2),(2, 0),(3,1))
    val end = permutations.zipWithIndex.map{case (a, b) => Bit(b, a)}

    //In every iteration of the fold we will add more permutations. In the list of perputations there is associated a snapshot of the Bits
    val initialValue = List(DefinitionSnapshot(List.empty[List[Char]], initialWithIndex.toList))
    val combinations= (0 to rounds).foldLeft(initialValue){ case (accum, round) =>
      accum.flatMap(a => a.permute)
    }
    combinations.filter(_.snapshot == end).map(_.permutations)
  }

}
