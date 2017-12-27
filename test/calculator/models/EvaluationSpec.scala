package calculator.models

import org.scalatest.WordSpec

class EvaluationSpec extends WordSpec {
  "Method solve" should {
    "return correct solution of expression without parenthesis" in {
      val expression1 = "15*3-10-20/10"
      val expression2 = "15*3/3"

      assert(Evaluation.solve(expression1) == Number("33"))
      assert(Evaluation.solve(expression2) == Number("15"))
    }
  }

  "Mathematical operations" should {
    "return correct solutions" in {
      val addition = Addition(Number("1"), Number("2"))
      val subtraction = Subtraction(Number("3"), Number("2"))
      val multiplication = Multiplication(Number("2"), Number("3"))
      val division = Division(Number("10"), Number("2"))

      assert(addition.solve().value == Number("3").value)
      assert(subtraction.solve().value == Number("1").value)
      assert(multiplication.solve().value == Number("6").value)
      assert(division.solve().value == Number("5").value)
    }
  }

  "Expression with nested parenthesis" should {
    "return correct solution" in {
      val expression1 = "(1+(2*3-1))+2"
      val expression2 = "(1+(2*(3+2)))"

      assert(Evaluation.solve(expression1) == Number("8"))
      assert(Evaluation.solve(expression2) == Number("11"))
    }
  }

  "Expression from task" should {
    "return correct value" in {
      val expression = "(1-1)*2+3*(1-3+4)+10/2"

      assert(Evaluation.solve(expression) == Number("11"))
    }
  }
}
