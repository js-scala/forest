package forest.compiler

trait Quote[-A] {
  def quote(a: A): String
}

object Quote {

  def apply[A : Quote]: Quote[A] = implicitly[Quote[A]]

  implicit object quoteString extends Quote[String] {
    override def quote(s: String) = "\"\"\"%s\"\"\"".format(s)
  }

  implicit def quoteNumber[A : Numeric] = new Quote[A] {
    override def quote(n: A) = n.toString
  }

  implicit def quoteList[A : Quote] = new Quote[List[A]] {
    override def quote(as: List[A]) = as match {
      case Nil => "List()"
      case _ => "List(%s)".format(as.map(Quote[A].quote(_)).mkString(", "))
    }
  }

  implicit def quoteMap[A : Quote, B : Quote] = new Quote[Map[A, B]] {
    override def quote(map: Map[A, B]) = {
      if (map.isEmpty) {
        "Map.empty"
      } else {
        "Map(%s)".format((for ((a, b) <- map) yield Quote[A].quote(a) + "->" + implicitly[Quote[B]].quote(b)).mkString(", "))
      }
    }
  }
  
  implicit def quoteOption[A : Quote] = new Quote[Option[A]] {
    override def quote(option: Option[A]) = option match {
      case None => "None"
      case Some(x) => "Some(%s)".format(Quote[A].quote(x))
    }
  }
}