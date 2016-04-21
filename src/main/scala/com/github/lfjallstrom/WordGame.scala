package com.github.lfjallstrom

object WordGame {

  def words(playground: Playground, dictionary: Trie) : Set[String] = {    
    case class Path(word: String, history: List[Position]) {
      def currentCol = history.head.col
      def currentRow = history.head.row
      def currentFloor = history.head.floor
    }

    // Here we construct the initial starting positions from where we start to
    // look for words
    val initialPaths = for {
      startFloor <- Stream.range(0, playground.floors)
      startRow <- Stream.range(0, playground.rows)
      startCol <- Stream.range(0, playground.cols)
      startPosition = Position(startCol, startRow, startFloor)
    } yield Path(playground.charAt(startPosition).toString, startPosition :: Nil)

    // This is a list of possible moves from a position
    val moves = for {
      df <- -1 to 1
      dr <- -1 to 1
      dc <- -1 to 1
      if df != 0 || dr != 0 || dc != 0
    } yield Position(dc, dr, df)

    // This build a lazy stream of possible paths to words
    def move(paths: Stream[Path]) : Stream[Stream[Path]] = {
      if (paths.isEmpty) {
        Stream.Empty
      }
      else {
        val next = for {
          // Generate the possible moves from a position
          path <- paths
          Position(dc, dr, df) <- moves
          nextPosition = Position(path.currentCol + dc, path.currentRow + dr, path.currentFloor + df)
          // Don't move outside the playground
          if (playground.isInside(nextPosition))
          // Don't go to a position which has already been visited
          if !path.history.contains(nextPosition)
          // Don't follow the path if it cannot be a word
          if dictionary.isPrefix(path.word + playground.charAt(nextPosition))
        } yield Path(path.word + playground.charAt(nextPosition), nextPosition :: path.history)
        
        // Prepend the potential paths to word to the stream
        paths #:: move(next)
      }
    }

    // Only take the actual words
    val words = for {
      paths <- move(initialPaths)
      path <- paths
      if dictionary.contains(path.word)
    } yield path

    words.map(_.word).toSet
  }
}