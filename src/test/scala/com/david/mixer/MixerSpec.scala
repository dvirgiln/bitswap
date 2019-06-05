package com.david.mixer

import org.scalatest.{ Matchers, WordSpec }

class MixerSpec extends WordSpec with Matchers {

  "Mixer" should {
    "return all the different combinations that in 4 steps achieve the final expected result" in {
      val expected = List(List('|', '|', '|'), List('|', '|', '|'), List('\\', '/', '|'), List('|', '\\', '/'))
      val rightShifts = List(2, 0, 0)
      val mixer = new Mixer(List(2, 0, 1), rightShifts)
      val result = mixer.findDefinitions(4, 4)
      result.contains(expected) should ===(true)

    }

    "return all the different combinations that in 6 steps achieve the final expected result" in {
      val mixer = new Mixer(List(3, 2, 0, 1), List(4, 3, 1, 1))
      val value = mixer.findDefinitions(6, 6)
      println(value)
      value.isDefined should ===(true)
    }
  }
}
