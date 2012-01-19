package forest

class SymbolGenerator {
  
  private[this] var i = 0
  
  def fresh(): String = {
    i = i + 1
    "__" + i
  }
}