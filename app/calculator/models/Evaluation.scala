package calculator.models

object Evaluation {

  //FIXME temporary solution
  val possibleOperations: Array[String] = Array("+", "-", "*", "/")
  val digits: Array[String] = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

  def parseString(expression: String): Array[Character] = {
    expression.map{char =>
      matchChar(char.toString())
    }.toArray
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
}

sealed trait Character {
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
sealed trait Operation {
  def solve
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
  override def solve(): Unit = {}
}

case class Expression(value: Array[Character])