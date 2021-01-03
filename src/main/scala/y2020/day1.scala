package y2020

import scala.io.Source

class getYear(year: Int, enums: Seq[Int]) {
  def lessThan(year: Int): Seq[Int] = {
    enums.filter(_ < year/2)
  }

  def greaterThan(year: Int): Seq[Int] = {
    enums.filter(_ >= year/2)
  }

  def eqYear(lesser: Int, greater: Int): Option[Int] = {
    val sum: Int = lesser + greater
    val product: Int = lesser * greater
    if (sum == year) Some(product) else None
  }

  def mapGreater(lesser: Int): Seq[Int] = {
    greaterThan(year).flatMap { eqYear(lesser, _) }
  }

  def get(): Int = {
    val answer = lessThan(year) flatMap { mapGreater }
    answer.head
  }
}

object day1 extends App{
  val src = Source.fromFile("C:\\Users\\Zack\\IdeaProjects\\advent\\src\\main\\scala\\y2020\\resources\\day1.txt")
  val input = src.getLines()
    .map ( _.toInt )
    .toSeq
  val answer = new getYear(2020, input)

  println(s"Two numbers: ${answer.get()}")
}
