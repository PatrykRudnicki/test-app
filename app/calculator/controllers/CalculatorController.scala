package calculator.controllers

import javax.inject.{Inject, Singleton}

import akka.util.Timeout
import akka.pattern.ask
import calculator.models.ExpressionJson
import calculator.actors.{CalculatorActorProvider, EvaluationExpression, EvaluationResult}
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, ControllerComponents}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

@Singleton
class CalculatorController @Inject()(cc: ControllerComponents,
                                     calculatorActor: CalculatorActorProvider)
                                    (implicit val ec: ExecutionContext) extends AbstractController(cc) {
  val actor = calculatorActor.get()
  implicit val defaultTimeout: Timeout = Timeout(5 seconds)

  def evaluate(): Action[ExpressionJson] = Action.async(parse.json[ExpressionJson]){ implicit request =>
    val expressionJson = request.body
    (actor ? EvaluationExpression(expressionJson)).mapTo[EvaluationResult].map { result =>
      Ok(Json.toJson(result))
    }
  }
}