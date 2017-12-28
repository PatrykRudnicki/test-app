package calculator.models

import scala.annotation.tailrec

object Evaluation {
  val possibleOperations: Array[String] = Array("+", "-", "*", "/")
  val digits: Array[String] = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  val operators: Array[Operator] = Array(Operator("*"), Operator("/"), Operator("-"), Operator("+"))

  def solve(expression: String): Number = {
    val solution = parseExpression(expression.replaceAll(" ", ""))
    val checkedParenthesis = solveParenthesis(solution)
    solveExpression(checkedParenthesis)
  }

  @tailrec
  private def parseExpression(expression: String, solutions: Array[Character] = Array()): Array[Character] = {
    if(expression.length == 0) {
      solutions
    } else {
        expression.head.toString() match {
        case character if(digits.contains(character)) => {
          val solution = Number(expression.takeWhile(char => char.isDigit).toInt)
          parseExpression(expression.drop(solution.value.toString().length), solutions ++ Array(solution))
        }
        case character if(possibleOperations.contains(character)) =>
          parseExpression(expression.tail, solutions ++ Array(Operator(character)))
        case "(" => parseExpression(expression.tail, solutions ++ Array(LeftParenthesis))
        case ")" => parseExpression(expression.tail, solutions ++ Array(RightParenthesis))
      }
    }
  }

  @tailrec
  private def solveParenthesis(expression: Array[Character]): Array[Character] = {
    if(expression.contains(LeftParenthesis)){
      val leftIndex = expression.lastIndexOf(LeftParenthesis)
      val rightIndex = expression.indexOf(RightParenthesis, leftIndex)
      val solution = solveExpression(expression.drop(leftIndex + 1).dropRight(expression.length - rightIndex))
      solveParenthesis(expression.take(leftIndex) ++ Array(solution) ++ expression.takeRight(expression.length - (rightIndex + 1)))
    } else {
      expression
    }
  }

  @tailrec
  private def solveExpression(expression: Array[Character], operators: Array[Operator] = operators): Number = {
    if(expression.length == 1) {
      expression(0).asInstanceOf[Number]
    } else {
      if(expression.contains(operators.head)){
        val index = expression.indexOf(operators.head)
        val solution = matchOperator(expression(index - 1).asInstanceOf[Number], operators.head, expression(index + 1).asInstanceOf[Number])
        if(expression.length > 4){
          index match {
            case _ if(index > 1 && index < expression.length - 3) =>
              solveExpression(expression.take(index-1) ++ Array(solution) ++ expression.takeRight(expression.length - (index+2)), operators)
            case _ if(index == 1) =>
              solveExpression(Array(solution) ++ expression.takeRight(expression.length - (index+2)), operators)
            case _ => solveExpression(expression.take(index-1) ++ Array(solution), operators)
          }
        } else {
          solution.asInstanceOf[Number]
        }
      } else {
        solveExpression(expression, operators.tail)
      }
    }
  }

  def matchOperator(elem1: Number, operator: Operator, elem2: Number): Number = {
    operator.value match {
      case "+" => Addition(elem1, elem2).solve()
      case "-" => Subtraction(elem1, elem2).solve()
      case "*" => Multiplication(elem1, elem2).solve()
      case "/" => Division(elem1, elem2).solve()
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