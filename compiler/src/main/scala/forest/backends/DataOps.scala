package forest.backends

trait DataOps {
  type Data
  
  def _iterate(data: Data): Iterable[Data]
  def _test(data: Data): Boolean
  def _show(data: Data): String
  def _get(data: Data, field: String): Data
}