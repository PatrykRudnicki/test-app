package calculator.actors

import javax.inject.{Inject, Provider}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import calculator.models._
import play.api.libs.json.Json

class CalculatorActor extends Actor {

  override def receive = {
    case EvaluationExpression(expression) =>
      calculateExpression(expression.expression)
  }

  private def calculateExpression(expression: String) = {
    val solution = Evaluation.solve(expression).value
    sender() ! EvaluationResult(solution)
  }

}

class CalculatorActorProvider @Inject()(system: ActorSystem) extends Provider[ActorRef]{
  def props() = Props(classOf[CalculatorActor])

  override def get(): ActorRef = system.actorOf(props())
}

case class EvaluationExpression(expression: ExpressionJson)

case class EvaluationResult(result: String)

object EvaluationResult {
  implicit val format = Json.format[EvaluationResult]
}
