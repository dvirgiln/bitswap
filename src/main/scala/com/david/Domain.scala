package com.david

object Domain {
  type DefinitionStage = List[Char]
  type Definition = List[DefinitionStage]
  case class Bit(index: Int, value: Int)
  case class DefinitionSnapshot(permutations: Definition, snapshot: List[Bit]){

    private val operations = List('|','/', '\\')

    def permute(): List[DefinitionSnapshot] = ???

    /*
    {
      snapshot.foldLeft(List.empty[DefinitionStage]){ case (accum, bit) =>
        accum
      }
      List.empty[]
    }
     */
  }
}
