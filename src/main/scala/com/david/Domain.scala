package com.david

object Domain {
  type DefinitionStage = List[Char]
  type Definition = List[DefinitionStage]
  case class Bit(index: Int, value: Int)
  case class DefinitionSnapshot(permutations: Definition, snapshot: List[Bit]){

    private val remainOp = (b: Bit, previous: Option[Char], index: Int, size: Int) => Some(b)
    private val shiftRightOp = (b: Bit, previous: Option[Char], index: Int, size: Int) => if(index<size-1) Some(Bit(b.index + 1,b.value)) else None
    private val shiftLeftOp = (b: Bit, previous: Option[Char], index: Int, size: Int) => previous match {
      case Some(p) if(p=='\\') => Some(Bit(b.index - 1,b.value))
      case _ => None
    }
    private val operations = Map('|' -> remainOp, '\\' -> shiftRightOp,'/' -> shiftLeftOp)

    def permute(): List[DefinitionSnapshot] =
    {
      val size = snapshot.size

      //This fold it is going to calculate all the definitions stage possible from the existing snapshot
      val newStages =snapshot.foldLeft((List.empty[(DefinitionStage, List[Bit])],0)){ case ((processedStages, index), bit) => processedStages match {
          case Nil =>
            val defAndBitList = operations.toList.map{ case (operation, func) => (List(operation),func(bit, None, index, size))}
            val filteredList = defAndBitList.filter(_._2.isDefined).map{case (o, b) => (o, List(b.get))}
            (filteredList, index + 1)
          case _ =>
            val combined = processedStages.flatMap{ case(stage, bits) =>
              val defAndBitList = operations.toList.map{ case (operation, func) => (operation,func(bit, Some(stage.last), index, size))}
              val filteredList = defAndBitList.filter(_._2.isDefined).map{case (o, b) => (o, b.get)}
              filteredList.map{case (character, bit) => (stage :+ character, bits :+ bit)}
            }
            (combined, index + 1)
        }
      }._1

      //Then we need to add the DefinitionStage (List[Char], List[Bit]) to the exsisting permutations to obtain the new list of permutations
      newStages.map{ case(s, b) => DefinitionSnapshot(permutations :+ s, b)}
    }

  }
}
