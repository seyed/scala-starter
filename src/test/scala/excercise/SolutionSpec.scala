package excercise

import exercise.Solution
import org.scalatest._

class SolutionSpec extends FlatSpec with Matchers {
  "The Hello object" should "say 'First test should pass!'" in {
    Solution.solve shouldEqual "First test should pass!"
  }
}