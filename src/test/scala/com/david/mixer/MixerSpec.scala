package com.david.mixer

import org.scalatest.{ Matchers, WordSpec }

class MixerSpec extends WordSpec with Matchers {

  "Mixer" should {
    "return all the different combinations that in 4 steps achieve the final expected result" in {
      val expected = List(List('|', '|', '|'), List('\\', '/', '|'), List('|', '\\', '/'), List('|', '|', '|'))
      val rightShifts = List(2, 0, 0)
      val result = Mixer.findDefinitions(List(2, 0, 1),rightShifts, 4, 4)
      result.contains(expected) should ===(true)
      result should ===(List(List(List('|', '|', '|'), List('|', '|', '|'), List('\\', '/', '|'), List('|', '\\', '/')),
        List(List('|', '|', '|'), List('\\', '/', '|'), List('|', '|', '|'), List('|', '\\', '/')),
        List(List('|', '|', '|'), List('\\', '/', '|'), List('|', '\\', '/'), List('|', '|', '|')),
        List(List('\\', '/', '|'), List('|', '|', '|'), List('|', '|', '|'), List('|', '\\', '/')),
        List(List('\\', '/', '|'), List('|', '|', '|'), List('|', '\\', '/'), List('|', '|', '|')),
        List(List('\\', '/', '|'), List('|', '\\', '/'), List('|', '|', '|'), List('|', '|', '|'))))
    }

    "return all the different combinations that in 6 steps achieve the final expected result" in {
      val expected = List(List('\\', '/', '\\', '/'), List('\\', '/', '|', '|'), List('\\', '/', '\\', '/'),
        List('|', '\\', '/', '|'), List('\\', '/', '\\', '/'), List('|', '\\', '/', '|'))

      Mixer.findDefinitions(List(3, 2, 0, 1), List(4,3,1,1), 6, 6).contains(expected) should ===(true)
    }
  }
}
