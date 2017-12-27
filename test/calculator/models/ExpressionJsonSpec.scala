package calculator.models

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsError, JsSuccess, Json}

class ExpressionJsonSpec extends WordSpec with Matchers {
  "Json expression" should {
    "be successfully parsed if correct" in {
      val expressionJson = Json.obj("expression" -> "1+1")

      ExpressionJson.reads.reads(expressionJson) shouldBe a[JsSuccess[_]]
    }

    "return JsError if expression is incorrect" in {
      val incorrectExpressionJson = Json.obj("expression" -> "a1d+4d")

      ExpressionJson.reads.reads(incorrectExpressionJson) shouldBe a[JsError]
    }
  }
}
