package calculator.models

object Evaluation {

  //FIXME temporary solution
  val possibleOperations: Array[String] = Array("+", "-", "*", "/")
  val digits: Array[String] = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

  def solve(expression: String): Number = {
    val parsedString = expression.map{char =>
      matchChar(char.toString())
    }.toArray
    val checkedNumbers = checkNumbers(parsedString)
    val checkedParenthesis = solveParenthesis(checkedNumbers)
    solveExpression(checkedParenthesis, Array(Operator("*"), Operator("/"), Operator("-"), Operator("+")))
  }

  private def matchChar(char: String): Character = {
    char match {
      case _ if(digits.contains(char)) => Number(char)
      case _ if(possibleOperations.contains(char)) => Operator(char)
      case "(" => LeftParenthesis
      case ")" => RightParenthesis
      case value => Error("Incorrect value: " + value.toString)
    }
  }

  private def checkNumbers(characters: Array[Character]): Array[Character] = {
    var stack = Array[Number]()
    var filteredCharacters = Array[Character]()
    characters.map{char: Character =>
      char match {
        case charValue: Number if(stack.length == 0) => stack = stack :+ charValue
        case charValue: Number => {
          filteredCharacters = filteredCharacters :+ Number(stack(0).value + charValue.value)
          stack = Array[Number]()
        }
        case charValue => {
          if(stack.length > 0) {
            filteredCharacters = filteredCharacters :+ stack(0)
          }
          stack = Array[Number]()
          filteredCharacters = filteredCharacters :+ charValue
        }
      }
    }
    filteredCharacters ++= stack
    filteredCharacters
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
              val solution = solveExpression(checkedExp, Array(Operator("*"), Operator("/"), Operator("-"), Operator("+")))
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
    var currentOperation = Operator("")
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
        case _ => ""
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
      case _ => IncorrectOperation("Incorrect operator")
    }
  }
}

sealed trait Character
sealed trait Operation {
  def solve(): Number
}

case class Number(value: String) extends Character
case class Operator(value: String) extends Character
case object LeftParenthesis extends Character
case object RightParenthesis extends Character

//FIXME types: .toInt, .toString etc... and errors (Error, IncorrectOperation)

case class Addition(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number((elem1.value.toInt + elem2.value.toInt).toString)
  }
}

case class Subtraction(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number((elem1.value.toInt - elem2.value.toInt).toString)
  }
}

case class Multiplication(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number((elem1.value.toInt * elem2.value.toInt).toString)
  }
}

case class Division(elem1: Number, elem2: Number) extends Operation {
  override def solve(): Number = {
    Number((elem1.value.toInt / elem2.value.toInt).toString)
  }
}

case class Error(value: String) extends Character

case class IncorrectOperation(value: String) extends Operation {
  override def solve(): Number = Number("")
}

case class Expression(value: Array[Character])