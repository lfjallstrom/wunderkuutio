package com.github.lfjallstrom

import scala.annotation.tailrec

object Trie {
  val empty = Trie(false, Map.empty[String, Trie])

  def apply(values: String*) : Trie = apply(values)

  def apply(values: TraversableOnce[String]) : Trie = {
    values.foldLeft(Trie.empty) { (trie, value) =>
      trie.append(value)
    }
  }
}

// A simple String trie
case class Trie(isValue: Boolean, children: Map[String, Trie]) extends Traversable[String] {
  def append(s: String) : Trie = {
    if (s.isEmpty) {
      this.copy(isValue = true)
    }
    else {
      val child = children.get(s.substring(0, 1)).getOrElse(Trie.empty).append(s.substring(1))
      this.copy(children = children.updated(s.substring(0, 1), child))
    }
  }
  
  def contains(s: String) : Boolean = {
    if (s.isEmpty) {
      isValue
    }
    else {
      children.get(s.substring(0, 1)).map(_.contains(s.substring(1))).getOrElse(false)
    }
  }

  def isPrefix(s: String) : Boolean = {
    s.isEmpty || children.get(s.substring(0, 1)).map(_.isPrefix(s.substring(1))).getOrElse(false)
  }
  
  override def foreach[U](f: String => U) : Unit = {
    def foreach(prefix: String, trie: Trie) : Unit = {
      if (trie.isValue || trie.children.isEmpty) f(prefix)

      for ((nextChar, child) <- trie.children) {
        foreach(prefix + nextChar, child)
      }
    }
    
    foreach("", this)
  }
}