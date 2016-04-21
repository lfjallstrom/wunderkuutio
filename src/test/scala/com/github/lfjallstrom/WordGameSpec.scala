package com.github.lfjallstrom

import org.scalatest._
import scala.io.Source

class WordGameSpec extends FlatSpec with Matchers {
  val dictionary = TestHelpers.loadDictionary("words.txt")

  "Words" should "be found from a trivial playground" in {
    val source =
      """
        |ap
        |uk"""

    val playground = TestHelpers.loadPlayground(source, 2, 2, 1)
    
    WordGame.words(playground, dictionary) should contain only ("p", "apu")
  }

  they should "find words from simple playground" in {
    val source =
      """
        |ap
        |uk
        |
        |ie
        |no"""
        
     val playground = TestHelpers.loadPlayground(source, 2, 2, 2)
    
    WordGame.words(playground, dictionary) should contain only (
      "p", "apu", "ape", "aie", "ani", "ane", "pai", "uni", "kai", "kun", "ken", "kop",
      "koi", "koe", "ien", "eka", "eno", "nuo", "ope", "oka", "auki", "akne", "ainu",
      "aine", "aino", "pako", "pano", "puin", "puna", "punk", "pune", "pian", "pika",
      "piko", "pino", "peni", "poka", "poni", "upea", "kapi", "kani", "kupo", "kuin", "kipu",
      "kina", "kino", "koni", "kone", "inka", "naku", "nupi", "nupo", "noki", "okei", "onki",
      "aukio", "pauke", "paine", "paino", "pukea", "punka", "punoa", "piano", "piena", "pinko",
      "peoni", "poika", "kaino", "kunpa", "kuona", "kinua", "keinu", "keino", "kopea", "nopea",
      "oikea", "onkia", "pukine", "puikea", "piukea", "pinkoa", "pekoni", "penkoa", "poikue",
      "kapine", "kuopia", "kipuna", "keinua", "kopina", "apukeino"
    )
  }

  it should "find words from huge playground" in {
    val playground = TestHelpers.loadPlaygroundFromResource("cube.txt", 4, 4, 4)

    val words = WordGame.words(playground, dictionary)
    
    words should have size(1177)
  }
}
