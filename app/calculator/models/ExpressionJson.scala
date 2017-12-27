package calculator.models

import play.api.libs.json._
import play.api.libs.json.Reads

case class ExpressionJson(expression: String)

object ExpressionJson {
  private def validateExpression()(implicit reads: Reads[String]): Reads[String] = {
    Reads[String](js => reads.reads(js).flatMap { expression =>
      val regex = """(^((\()|[0-9])+(\(|[0-9]|\)|\*|\+|\-|\/)+((\))|[0-9])$)""".r
      regex.unapplySeq(expression).map(_ =>  JsSuccess(expression)).getOrElse(JsError("Invalid expression"))
    })
  }

  val reads: Reads[ExpressionJson] = ((JsPath \ "expression").read[String](validateExpression())).map(ExpressionJson.apply _)

  implicit val format = Format.apply(reads, Json.writes[ExpressionJson])
}