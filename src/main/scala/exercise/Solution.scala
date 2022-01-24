package exercise

import java.io.Console
import scala.util.Random

object Solution extends App {

  sealed trait Check {
    def guess: Guess
  }

  final case class High(guess: Guess) extends Check

  final case class Low(guess: Guess) extends Check

  final case class Equal(guess: Guess) extends Check

  case class Difficulty(low: Int, high: Int)

  val easy = Difficulty(1, 10)
  val medium = Difficulty(1, 100)
  val hard = Difficulty(1, 1000)

  case class InputOutOfBounds(msg: String) extends Exception

  case class Guess(value: Int, tries: Int)

  def getRandom(low: Int, high: Int): Int = Random.between(low, high + 1)

  def isHighLowOrEqual(guess: Guess, expected: Int): Check = (guess, expected) match {
    case (g, e) if g.value > e => High(g.copy(tries = g.tries + 1))
    case (g, e) if g.value < e => Low(g.copy(tries = g.tries + 1))
    case (g, e) if g.value == e => Equal(g.copy(tries = g.tries + 1))
  }

  def getDifficulty(input: String): Either[InputOutOfBounds, Difficulty] = {
    val difficulties = Map(
      "1" -> easy,
      "2" -> medium,
      "3" -> hard
    )
    difficulties.get(input)
      .fold[Either[InputOutOfBounds, Difficulty]](
        Left(InputOutOfBounds("omg")))(
        Right(_)
      )
  }

  def finishGame(guess: Guess): String = guess match {
    case g if g.tries == 1 => "Perfect score"
    case g if g.tries >= 2 && g.tries <= 4 => "Very good"
    case g if g.tries >= 5 && g.tries <= 6 => "Not bad"
    case g if g.tries >= 7 => "need to try harder next round! :-)"
  }

  def runGame(console: Console): String = ???


  def guessLoop(guesses: List[String], value: Int): String = {
    var currentGuess = Guess(guesses.head.toInt, 0)
    guesses.foreach{ g =>
      currentGuess = isHighLowOrEqual(Guess(g.toInt, currentGuess.tries), value).guess
    }
    finishGame(currentGuess)
  }

}