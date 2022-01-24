package excercise

import exercise.Solution._
import org.scalatest._

import java.io.ByteArrayInputStream
import scala.io.StdIn

class SolutionSpec extends FlatSpec with Matchers {

  "getRandom" should "return a random Int between high and low (inclusive)" in {
    val result = getRandom(1, 10)

    result should be <= 10
    result should be >= 1
  }

  "isHighLowOrEqual" should "return high when guess is above given number and guess is incremented" in {
    isHighLowOrEqual(Guess(10, 0), 7) shouldEqual High(Guess(10, 1))
  }

  "isHighLowOrEqual" should "return low when guess is above given number" in {
    isHighLowOrEqual(Guess(1, 4), 7) shouldEqual Low(Guess(1, 5))
  }

  "isHighLowOrEqual" should "return equal when guess is above given number" in {
    isHighLowOrEqual(Guess(7, 8), 7) shouldEqual Equal(Guess(7, 9))
  }

  "getDifficulty" should "get easy difficulty for input 1" in {
    getDifficulty("1") shouldEqual Right(easy)
  }

  "getDifficulty" should "get medium difficulty for input 2" in {
    getDifficulty("2") shouldEqual Right(medium)
  }

  "getDifficulty" should "get hard difficulty for input 1" in {
    getDifficulty("3") shouldEqual Right(hard)
  }

  "getDifficulty" should "get error" in {
    getDifficulty("1000") shouldEqual Left(InputOutOfBounds("omg"))
  }

  "finishGame" should "return perfect score if tries == 1" in {
    finishGame(Guess(1, 1)) shouldEqual "Perfect score"
  }

  "finishGame" should "return very good if 2 <= tries <= 4" in {
    finishGame(Guess(1, 2)) shouldEqual "Very good"
    finishGame(Guess(1, 3)) shouldEqual "Very good"
    finishGame(Guess(1, 4)) shouldEqual "Very good"
  }

  "finishGame" should "return not bad if 5 <= tries <= 6" in {
    finishGame(Guess(1, 5)) shouldEqual "Not bad"
    finishGame(Guess(1, 6)) shouldEqual "Not bad"
  }

  "finishGame" should "return try harder if tries >= 7" in {
    finishGame(Guess(1, 7)) shouldEqual "need to try harder next round! :-)"
  }

  "run" should "" in {

    val invalidInput = new ByteArrayInputStream(("abc").getBytes)
    Console.withIn(invalidInput) {
      getDifficulty(StdIn.readLine()) shouldEqual Left(InputOutOfBounds("omg"))
    }

    val easyInput = new ByteArrayInputStream(("1").getBytes)
    Console.withIn(easyInput) {
      getDifficulty(StdIn.readLine()) shouldEqual Right(easy)
      getDifficulty(StdIn.readLine()) shouldNot equal(Right(hard))
    }

    val mediumInput = new ByteArrayInputStream(("2").getBytes)
    Console.withIn(mediumInput) {
      getDifficulty(StdIn.readLine()) shouldEqual Right(medium)
    }

    val hardInput = new ByteArrayInputStream(("3").getBytes)
    Console.withIn(hardInput) {
      getDifficulty(StdIn.readLine()) shouldEqual Right(hard)
    }

    //    val stream = new java.io.ByteArrayOutputStream()
    //    Console.withOut(stream) {
    //      println("Fly me to the moon, let me play among the stars")
    //    }


    //mock.readline
    //mock.println
    //
    // keep state
    // mock.readline
    //mock.println


  }

  "guessLoop" should "return final score for perfect guess" in {
    guessLoop(
      List(
        "5"
      ),
      easy,
      5
    ) shouldEqual "Perfect score"
  }

}