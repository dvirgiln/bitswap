package com.david

import org.scalatest.{ Matchers, WordSpec }

class MixerSpec extends WordSpec with Matchers {

  "Mixer" should {
    "return all the different combinations that in 4 steps achieve the final expected result" in {
      val expected = List('|','|','|') :: List('\\','/','|') :: List('|','\\','/') :: List('|','|','|') :: Nil

      Mixer.combinations(List(2, 0, 1), 4).contains(expected) should ===(true)
    }
    "return all the different combinations that in 6 steps achieve the final expected result" in {
      val expected = List('\\','/','\\', '/') :: List('\\','/','|', '|') :: List('\\','/','\\', '/') ::
        List('|','\\','/', '|') :: List('\\','/','\\', '/') :: List('|','\\','/', '|') :: Nil

      Mixer.combinations(List(2, 2, 0, 1), 6).contains(expected) should ===(true)
    }
  }
}
