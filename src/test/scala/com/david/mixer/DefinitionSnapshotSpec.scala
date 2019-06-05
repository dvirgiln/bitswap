package com.david.mixer

import Domain._
import org.scalatest.{ Matchers, WordSpec }

class DefinitionSnapshotSpec extends WordSpec with Matchers {

  "Definition snapshot" should {
    "return find the different combinations for the bits 0 2 1" in {
      val snapshot = new DefinitionSnapshot(List.empty[String], List(Bit(0, 0), Bit(1, 2), Bit(2, 1)))
      val rightShifts = Map(0 -> 5, 1 -> 5, 2 -> 5)
      val expectedBits: Map[Int, Bit] = Map(0 -> Bit(2, 0, 5), 1 -> Bit(1, 1, 5), 2 -> Bit(0, 2, 5))
      val result = snapshot.calculateSnapshotPermutation(rightShifts, expectedBits, 5)
      result should ===(List(
        (("|||"), List(Bit(0, 0, 0), Bit(1, 2, 0), Bit(2, 1, 0))),
        (("|\\/"), List(Bit(0, 0, 0), Bit(2, 2, 1), Bit(1, 1, 0))),
        (("\\/|"), List(Bit(1, 0, 1), Bit(0, 2, 0), Bit(2, 1, 0)))
      ))
    }
  }
}
