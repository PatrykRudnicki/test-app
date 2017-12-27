package calculator.models

import org.scalatest.WordSpec

class EvaluationSpec extends WordSpec {
  "Evaluation" should {
    "parse Json correctly" in {
      val expressionJson = "1+1"
      val parsedString: Array[Character] = Evaluation.parseString(expressionJson)

      assert(parsedString(0) == Number("1"))
      assert(parsedString(1) == Operator("+"))
      assert(parsedString(2) == Number("1"))
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
}
