package y2020

import scala.io.Source

class treeCounter(val geography: Seq[String], val charIncrease: Int, val rowIncrease: Int) {
  private var rowIx: Int = 0
  private var charIx: Int = 0
  private val finishRow: Int = geography.length
  private val outOfBounds: Int = geography.head.length

  private def increaseDepth(): Unit = {
    rowIx += rowIncrease
    charIx += charIncrease
    if (charIx >= outOfBounds) charIx = charIx - outOfBounds
  }

  private def checkTree(row: Int, char: Int): Int = {
    if (geography(row).charAt(char) == '#') 1
    else 0
  }

  def countTrees(): Long = {
    var numTrees = 0
    while(rowIx < finishRow) {
      numTrees += checkTree(rowIx, charIx)
      increaseDepth()
    }
    numTrees.toLong
  }
}

object day3 extends App {
  val src = Source.fromFile("C:\\Users\\Zack\\IdeaProjects\\advent\\src\\main\\scala\\y2020\\resources\\day3.txt")
  val input: Seq[String] = src.getLines().toSeq

  val configurations = Seq(
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
  )

  val numTrees = configurations.map { config =>
    val trees = new treeCounter(input, config._1, config._2)
    trees.countTrees()
  }

  println(configurations.zip(numTrees))
  println(numTrees.product)
}
