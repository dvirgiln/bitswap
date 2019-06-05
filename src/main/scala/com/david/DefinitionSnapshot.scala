package com.david

import com.david.Domain.{Bit, Definition, DefinitionStep}

object StepOperations{
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
  val operations = Map('|' -> remainOp, '\\' -> shiftRightOp,'/' -> shiftLeftOp)
}


class DefinitionSnapshot(val steps: Definition, val snapshot: List[Bit]){

  import StepOperations._
  /*
   * This function is the one that calculate all the possible permutations/operations than can be applied from the current
   * definition snapshot. It has to go bit by bit checking the possible operations to apply
   */
  def calculateSnapshotPermutation={
    val size = snapshot.size
    //This fold it is going to calculate all the definitions stage possible from the existing snapshot
    snapshot.foldLeft((List.empty[(DefinitionStep, List[Bit])],0)){ case ((accumulatedValue, index), bit) => accumulatedValue match {
      case Nil => //There is not a previous accumulated value
        val funcApplied = operations.toList.map{ case (operation, func) => (List(operation),func(bit, None, index, size))}
        val filteredList: List[(List[Char], List[Bit])] = funcApplied.filter(_._2.isDefined).map{case (o, b) => (o, List(b.get))}
        //Returning the first combination of Bits, Operations
        (filteredList, index + 1)
      case _ => // At least one bit has been calculated
        //It is required to iterate over the accumulated value and get the previous value of every step
        val combined = accumulatedValue.flatMap{ case(step, bits) =>
          //Applying the operations including the last character from the step
          val funcApplied = operations.toList.map{ case (operation, func) => (operation,func(bit, Some(step.last), index, size))}
          val filteredList = funcApplied.filter(_._2.isDefined).map{case (o, b) => (o, b.get)}
          //Creating a list of the existing step adding the new character and the same for the bits
          filteredList.map{case (character, bit) => (step :+ character, bits :+ bit)}
        }
        (combined, index + 1)
    }
    }._1
  }

  /*
   * This function calculates all the possible steps that can be added from the existing Definition Step.
   * Then it append them to the existing definition snapshot and generate a new Definition Snapshot, with the new bits
   */
  def concatenateStep(): List[DefinitionSnapshot] =
  {
    val newStages = calculateSnapshotPermutation
    //Then we need to add the DefinitionStage (List[Char], List[Bit]) to the exsisting permutations to obtain the new list of permutations
    newStages.map{ case(s, b) => new DefinitionSnapshot(steps :+ s, b.sortBy(_.index))}
  }

}
