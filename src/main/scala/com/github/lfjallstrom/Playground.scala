package com.github.lfjallstrom

case class Position(col: Int, row: Int, floor: Int)

class Playground(private val grid: IndexedSeq[IndexedSeq[IndexedSeq[Char]]]) {
  val cols = grid(0)(0).length
  val rows = grid(0).length
  val floors = grid.length

  def charAt(position: Position) : Char = grid(position.floor)(position.row)(position.col)

  def isInside(position: Position) : Boolean = {
    0 <= position.col && position.col < cols &&
    0 <= position.row && position.row < rows &&
    0 <= position.floor && position.floor < floors
  }
}

object Playground {
  def parse(source: List[String], cols: Int, rows: Int, floors: Int) : Playground = {
    assert(source.forall(_.length() == cols))
    assert(source.length == rows * floors)

    val s = source.mkString("").toLowerCase()

    val v = for {
      floor <- 0 until floors
    } yield for {
      row <- 0 until rows
    } yield for {
      col <- 0 until cols
    } yield s.charAt(floor * rows * cols + row * cols + col)

    new Playground(v)
  }
}
