package y2020

import scala.io.Source

class ParsedRow(val password: String,
                val letterRule: Char,
                val minAllowed: Int,
                val maxAllowed: Int
               ) {
}

trait PasswordVerifier {
  def apply(password: String,
    letterRule: Char,
    minAllowed: Int,
    maxAllowed: Int
  ): Boolean
}

object PasswordVerifierV1 extends PasswordVerifier {
  def apply(password: String,
            letterRule: Char,
            minAllowed: Int,
            maxAllowed: Int
           ): Boolean = {
    val numMatches: Int = password.count( _ == letterRule)
    numMatches >= minAllowed && numMatches <= maxAllowed
  }
}

object PasswordVerifierV2 extends PasswordVerifier {
  def apply(password: String,
            letterRule: Char,
            minAllowed: Int,
            maxAllowed: Int
           ): Boolean = {

    val first: Boolean = password.charAt(minAllowed - 1) == letterRule
    val second: Boolean = password.charAt(maxAllowed - 1) == letterRule

    if (!(first || second)) false
    else if (first && second) false
    else true
  }
}

object Parser {
  def apply(input: String): ParsedRow = {
    val split: Array[String] = input.split(" ")

    val numAllowed: Array[Int] = split(0).split("-").map( _.toInt )

    val minAllowed: Int = numAllowed(0)
    val maxAllowed: Int = numAllowed(1)

    val letter: Char = split(1).replaceAll(":", "").charAt(0)

    val password: String = split(2)

    new ParsedRow(password, letter, minAllowed, maxAllowed)
  }
}

object day2 extends App {
  val src = Source.fromFile("C:\\Users\\Zack\\IdeaProjects\\advent\\src\\main\\scala\\y2020\\resources\\day2.txt")
  val input: Seq[String] = src.getLines().toSeq

  val parsed: Seq[ParsedRow] = input.map { Parser(_) }

  val numValidV1: Int = parsed.map { contents => {
        PasswordVerifierV1(
          contents.password,
          contents.letterRule,
          contents.minAllowed,
          contents.maxAllowed
        )
      }
    }.map{ b => if(b) 1 else 0 }.sum

  val numValidV2: Int = parsed.map { contents => {
        PasswordVerifierV2(
          contents.password,
          contents.letterRule,
          contents.minAllowed,
          contents.maxAllowed
        )
      }
    }.map{ b => if(b) 1 else 0 }.sum

  println(s"Number valid passwords v1: $numValidV1")
  println(s"Number valid passwords v2: $numValidV2")
}
