package com.github.lfjallstrom

import org.scalatest._

class PlaygroundSpec extends FlatSpec with Matchers {
  "Playground" should "be constructed from trivial source" in {
    val source =
      """
        |ap
        |uk"""

    val playground = TestHelpers.loadPlayground(source, 2, 2, 1)

    playground.charAt(Position(0, 0, 0)) should be ('a')
    playground.charAt(Position(1, 0, 0)) should be ('p')
    playground.charAt(Position(0, 1, 0)) should be ('u')
    playground.charAt(Position(1, 1, 0)) should be ('k')
  }
  
  it should "be constructed from a simple cube" in {
    val source =
      """
        |ap
        |uk
        |
        |ie
        |no"""
    
    val playground = TestHelpers.loadPlayground(source, 2, 2, 2)

    playground.charAt(Position(0, 0, 0)) should be ('a')
    playground.charAt(Position(1, 0, 0)) should be ('p')
    playground.charAt(Position(0, 1, 0)) should be ('u')
    playground.charAt(Position(1, 1, 0)) should be ('k')

    playground.charAt(Position(0, 0, 1)) should be ('i')
    playground.charAt(Position(1, 0, 1)) should be ('e')
    playground.charAt(Position(0, 1, 1)) should be ('n')
    playground.charAt(Position(1, 1, 1)) should be ('o')
  }
}