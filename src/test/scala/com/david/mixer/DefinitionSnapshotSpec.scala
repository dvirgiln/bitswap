package com.david.mixer

import Domain._
import org.scalatest.{ Matchers, WordSpec }

class DefinitionSnapshotSpec extends WordSpec with Matchers {

  "Definition snapshot" should {
    "return find the different combinations for the bits 0 2 1" in {
      val snapshot = new DefinitionSnapshot(List.empty[DefinitionStep], List(Bit(0, 0), Bit(1, 2), Bit(2, 1)))
      val result = snapshot.calculateSnapshotPermutation
      result should ===(List(
        (List('|', '|', '|'), List(Bit(0, 0), Bit(1, 2), Bit(2, 1))),
        (List('|', '\\', '/'), List(Bit(0, 0), Bit(2, 2), Bit(1, 1))),
        (List('\\', '/', '|'), List(Bit(1, 0), Bit(0, 2), Bit(2, 1)))
      ))
    }
  }
}
