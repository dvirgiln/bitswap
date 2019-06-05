package com.david

import com.david.Domain.{Bit, Definition, DefinitionStep}

class DefinitionSnapshot(val steps: Definition, val snapshot: List[Bit]){

  /*
    Operations to shift the bits. It depends on the previous value and in the index position.
    All of them they return an Option. In case the operation cannot be applied None would be returned.
   */
  private val remainOp = (b: Bit, previous: Option[Char], index: Int, size: Int) => previous match {
    case Some(p) if(p=='\\') => None
    case _ => Some(b)
  }
  private val shiftRightOp = (b: Bit, previous: Option[Char], index: Int, size: Int) => previous match {
    case Some(p) if(p=='\\') => None
    case _ =>
      if(index<size-1) Some(Bit(b.index + 1,b.value)) else None
  }

  private val shiftLeftOp = (b: Bit, previous: Option[Char], index: Int, size: Int) => previous match {
    case Some(p) if(p=='\\') => Some(Bit(b.index - 1,b.value))
    case _ => None
  }

  //List of all the operations and the character associated to the operation
  private val operations = Map('|' -> remainOp, '\\' -> shiftRightOp,'/' -> shiftLeftOp)

  def calculateSnapshotPermutation={
    val size = snapshot.size
    //This fold it is going to calculate all the definitions stage possible from the existing snapshot
    snapshot.foldLeft((List.empty[(DefinitionStep, List[Bit])],0)){ case ((accumulatedValue, index), bit) => accumulatedValue match {
      case Nil => //There is not a previous accumulated value
        val funcApplied = operations.toList.map{ case (operation, func) => (List(operation),func(bit, None, index, size))}
        val filteredList: List[(List[Char], List[Bit])] = funcApplied.filter(_._2.isDefined).map{case (o, b) => (o, List(b.get))}
        //Returning the first combination of Bits, Operations
        (filteredList, index + 1)
      case _ =>
        //It is required to iterate over the accumulated value and get the previous value of every stage
        val combined = accumulatedValue.flatMap{ case(stage, bits) =>
          //Applying the operations including the last character from the stage
          val funcApplied = operations.toList.map{ case (operation, func) => (operation,func(bit, Some(stage.last), index, size))}
          val filteredList = funcApplied.filter(_._2.isDefined).map{case (o, b) => (o, b.get)}
          //Creating a list of the existing stage adding the new character and the same for the bits
          filteredList.map{case (character, bit) => (stage :+ character, bits :+ bit)}
        }
        (combined, index + 1)
    }
    }._1
  }

  def concatenateStep(): List[DefinitionSnapshot] =
  {
    val newStages = calculateSnapshotPermutation
    //Then we need to add the DefinitionStage (List[Char], List[Bit]) to the exsisting permutations to obtain the new list of permutations
    newStages.map{ case(s, b) => new DefinitionSnapshot(steps :+ s, b.sortBy(_.index))}
  }

}
