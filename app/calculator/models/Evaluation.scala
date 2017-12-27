package calculator.models

object Evaluation {
  val possibleOperations: Array[String] = Array("+", "-", "*", "/")
  val digits: Array[String] = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  val operators: Array[Operator] = Array(Operator("*"), Operator("/"), Operator("-"), Operator("+"))

  def solve(expression: String): Number = {
    val solution = parseExpression(expression)
    val checkedParenthesis = solveParenthesis(solution)
    solveExpression(checkedParenthesis, operators)
  }

  private def parseExpression(expression: String): Array[Character] = {
    var stack = Array[String]()
    var solution = Array[Character]()

    def clearStack() = {
      if(!stack.isEmpty){
        solution = solution :+ Number(stack.mkString("").toInt)
        stack = Array[String]()
      }
    }

    expression.replaceAll(" ", "").map{ char =>
      char.toString() match {
        case character if(digits.contains(character)) => {
          stack = stack :+ character
        }
        case character if(possibleOperations.contains(character)) => {
          clearStack()
          solution = solution :+ Operator(character)
        }
        case "(" => {
          clearStack()
          solution = solution :+ LeftParenthesis
        }
        case ")" => {
          clearStack()
          solution = solution :+ RightParenthesis
        }
      }
    }
    clearStack()
    solution
  }

  private def solveParenthesis(expression: Array[Character]): Array[Character] = {
    if(expression.contains(LeftParenthesis) || expression.contains(RightParenthesis)){
      var stack = Array[Character]()
      var filteredExpression = Array[Character]()
      var isInParenthesis = false
      var numberOfParenthesis = 0

      expression.map { elem =>
        elem match {
          case _: LeftParenthesis.type => {
            if(numberOfParenthesis > 0) {
              stack = stack :+ elem
            }
            numberOfParenthesis = numberOfParenthesis + 1
            isInParenthesis = true
          }
          case _: RightParenthesis.type => {
            if(numberOfParenthesis > 1) {
              numberOfParenthesis = numberOfParenthesis - 1
              stack = stack :+ elem
            } else {
              val checkedExp = solveParenthesis(stack)
              val solution = solveExpression(checkedExp, operators)
              stack = Array[Character]()
              filteredExpression = filteredExpression :+ solution
              numberOfParenthesis = 0
              isInParenthesis = false
            }
          }
          case _: Character if(isInParenthesis) => {
            stack = stack :+ elem
          }
          case _: Character => filteredExpression = filteredExpression :+ elem
        }
      }
      filteredExpression
    } else {
      expression
    }
  }

  private def solveExpression(expression: Array[Character], operators: Array[Operator]): Number = {
    var stack = Array[Character]()
    var currentOperation: Operator = null
    var isOperation = false

    expression.map { elem =>
      elem match {
        case number: Number => {
          if(isOperation){
            val solution = matchMatchOperator(stack(0).asInstanceOf[Number], currentOperation,elem.asInstanceOf[Number]).solve()
            stack = stack.drop(1)
            stack = stack.+:(solution)
            isOperation = false
          } else {
            stack = stack.+:(number)
          }
        }
        case operator: Operator => {
          if(operator.value == operators(0).value) {
            currentOperation = operator
            isOperation = true
          } else {
            stack = stack.+:(operator)
          }
        }
      }
    }
    if(operators.length > 0){
      solveExpression(stack.reverse, operators.tail)
    } else {
     stack(0).asInstanceOf[Number]
    }
  }

  def matchMatchOperator(elem1: Number, operator: Operator, elem2: Number): Operation = {
    operator.value match {
      case "+" => Addition(elem1, elem2)
      case "-" => Subtraction(elem1, elem2)
      case "*" => Multiplication(elem1, elem2)
      case "/" => Division(elem1, elem2)
    }
  }
}

sealed trait Character
sealed trait Operation {
  def solve(): Number
}

case class Number(value: Int) extends Character
case class Operator(value: String) extends Character
case object LeftParenthesis extends Character
case object RightParenthesis extends Character

case class Addition(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number(elem1.value + elem2.value)
  }
}

case class Subtraction(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number(elem1.value - elem2.value)
  }
}

case class Multiplication(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number(elem1.value * elem2.value)
  }
}

case class Division(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number(elem1.value / elem2.value)
  }
}