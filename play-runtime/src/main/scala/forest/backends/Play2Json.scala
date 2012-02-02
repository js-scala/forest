package forest.backends

import play.api.libs.json.{JsValue, JsNumber, JsArray, JsString, JsObject, JsBoolean, JsNull, JsUndefined}

trait Play2Json extends JsonOps {
  type Json = JsValue

  override def _iterate(json: JsValue): Iterable[JsValue] = json match {
    case JsArray(jsons) => jsons
    case _ => sys.error("Array expected")
  }
  
  override def _test(json: JsValue): Boolean = json match {
    case JsBoolean(b) => b
    case JsNumber(x) => x != 0
    case JsString(s) => !s.isEmpty
    case JsArray(js) => !js.isEmpty
    case JsObject(_) => true
    case JsNull | JsUndefined(_) => false
  }
  
  override def _show(json: JsValue): String = json match {
    case JsBoolean(b) => b.toString
    case JsNumber(x) => x.toString
    case JsString(s) => s
    case JsArray(js) => (js map _show).mkString(" ")
    case JsObject(o) => sys.error("Unable to show a Json object")
    case JsNull | JsUndefined(_) => ""
  }
  
  override def _get(json: JsValue, field: String) = json \ field
}