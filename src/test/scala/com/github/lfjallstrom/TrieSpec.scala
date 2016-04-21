package com.github.lfjallstrom

import org.scalatest._
import scala.io.Source

class TrieSpec extends FlatSpec with Matchers {
  "Trie" should "be constructed from a list of words" in {
    val dictionary = Trie("foo", "bar", "baz", "foobar")

    dictionary should contain allOf ("foo", "bar", "baz", "foobar")
    dictionary should not contain ("qux")
    dictionary should not contain ("fooba")
  }
  
  it should "be able to contain lots of words" in {
    val lines = Source.fromURL(getClass.getClassLoader.getResource("words.txt"), "UTF-8").getLines.toSet
    val dictionary = Trie(lines)

    dictionary should have size(93198)
    dictionary.forall(lines.contains) should be (true)
  }
  
  it should "know wether or not a string is a prefix of a complete word" in {
    val dictionary = Trie("foo", "bar", "baz", "foobar")
    
    dictionary.isPrefix("f") should be (true)
    dictionary.isPrefix("fo") should be (true)
    dictionary.isPrefix("foo") should be (true)
    dictionary.isPrefix("ba") should be (true)
    dictionary.isPrefix("q") should be (false)
    dictionary.isPrefix("barr") should be (false)
  }
}
