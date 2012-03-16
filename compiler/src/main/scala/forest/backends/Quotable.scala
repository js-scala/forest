package forest.backends

trait Quotable[A] {
  def quote(a: A): String
}

object Quotable {

  implicit val quoteString = new Quotable[String] {
    override def quote(s: String) = "\"\"\"%s\"\"\"".format(s)
  }

  implicit val quoteNumber = new Quotable[Number] {
    override def quote(n: Number) = n.toString
  }

  implicit def quoteList[A : Quotable] = new Quotable[List[A]] {
    override def quote(as: List[A]) = as match {
      case Nil => "Nil"
      case _ => "List(%s)".format(as.map(implicitly[Quotable[A]].quote(_)).mkString(", "))
    }
  }
}