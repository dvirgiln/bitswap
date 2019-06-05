package com.david.mixer

object Domain {
  type DefinitionStep = List[Char]
  type Definition = List[DefinitionStep]
  case class Bit(index: Int, value: Int, rightShifts: Int=0)
}
