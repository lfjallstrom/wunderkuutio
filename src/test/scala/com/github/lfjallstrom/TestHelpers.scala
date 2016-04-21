package com.github.lfjallstrom

import scala.io.Source

object TestHelpers {
  def loadPlaygroundFromResource(path: String, cols: Int, rows: Int, floors: Int) : Playground = {
    val source = Source.fromURL(getClass.getClassLoader.getResource("cube.txt"), "UTF-8")
    loadPlayground(source.getLines.toList, cols, rows, floors)
  }
  
  def loadPlayground(source: String, cols: Int, rows: Int, floors: Int) : Playground = {
    loadPlayground(source.stripMargin.lines.map(_.stripLineEnd).filterNot(_.isEmpty).toList, cols, rows, floors)
  }

  def loadPlayground(source: List[String], cols: Int, rows: Int, floors: Int) : Playground = {
    Playground.parse(source.map(_.stripLineEnd).filterNot(_.isEmpty), cols, rows, floors)
  }
  
  def loadDictionary(source: String) : Trie = {
    val lines = Source.fromURL(getClass.getClassLoader.getResource("words.txt"), "UTF-8").getLines
    Trie(lines)
  }
}