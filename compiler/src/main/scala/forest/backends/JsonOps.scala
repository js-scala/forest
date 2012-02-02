package forest.backends

trait JsonOps {
  type Json
  
  def _iterate(json: Json): Iterable[Json]
  def _test(json: Json): Boolean
  def _show(json: Json): String
  def _get(json: Json, field: String): Json
}