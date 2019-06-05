package com.david

import org.scalatest.{ Matchers, WordSpec }

class MixerSpec extends WordSpec with Matchers {

  "Mixer" should {
    "return all the different combinations that in 4 steps achieve the final expected result" in {
      val expected = List(List('|','|','|'), List('\\','/','|'), List('|','\\','/'),List('|','|','|'))
      val result = Mixer.combinations(List(2, 0, 1), 4)
      result.contains(expected) should ===(true)
      result should ===(List(
        List(List('|', '|', '|'), List('|', '|', '|'), List('\\', '/', '|'), List('|', '\\', '/')),
        List(List('|', '|', '|'), List('\\', '/', '|'), List('|', '|', '|'), List('|', '\\', '/')),
        List(List('|', '|', '|'), List('\\', '/', '|'), List('|', '\\', '/'), List('|', '|', '|')),
        List(List('|', '\\', '/'), List('|', '\\', '/'), List('\\', '/', '|'), List('|', '\\', '/')),
        List(List('|', '\\', '/'), List('\\', '/', '|'), List('|', '\\', '/'), List('\\', '/', '|')),
        List(List('\\', '/', '|'), List('|', '|', '|'), List('|', '|', '|'), List('|', '\\', '/')),
        List(List('\\', '/', '|'), List('|', '|', '|'), List('|', '\\', '/'), List('|', '|', '|')),
        List(List('\\', '/', '|'), List('|', '\\', '/'), List('|', '|', '|'), List('|', '|', '|')),
        List(List('\\', '/', '|'), List('|', '\\', '/'), List('|', '\\', '/'), List('|', '\\', '/')),
        List(List('\\', '/', '|'), List('|', '\\', '/'), List('\\', '/', '|'), List('\\', '/', '|')),
        List(List('\\', '/', '|'), List('\\', '/', '|'), List('\\', '/', '|'), List('|', '\\', '/'))))
    }


    "return all the different combinations that in 6 steps achieve the final expected result" in {
      val expected = List(List('\\','/','\\', '/'), List('\\','/','|', '|'), List('\\','/','\\', '/'),
        List('|','\\','/', '|'), List('\\','/','\\', '/'), List('|','\\','/', '|'))
      val result = Mixer.combinations(List(3, 2, 0, 1), 6)
      println(result)
      result.contains(expected) should ===(true)
    }
  }
}
