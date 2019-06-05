package com.david.mixer

object Domain {
  type Definition = List[String]

  case class Bit(index: Int, value: Int, rightShifts: Int = 0)

}
