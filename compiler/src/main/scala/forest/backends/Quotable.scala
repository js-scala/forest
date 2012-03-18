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

  implicit def quoteMap[A : Quotable, B : Quotable] = new Quotable[Map[A, B]] {
    override def quote(map: Map[A, B]) = {
      if (map.isEmpty) {
        "Map.empty"
      } else {
        "Map(%s)".format((for ((a, b) <- map) yield implicitly[Quotable[A]].quote(a) + "->" + implicitly[Quotable[B]].quote(b)).mkString(", "))
      }
    }
  }
  
  implicit def quoteOption[A : Quotable] = new Quotable[Option[A]] {
    override def quote(option: Option[A]) = option match {
      case None => "None"
      case Some(x) => "Some(%s)".format(implicitly[Quotable[A]].quote(x))
    }
  }
}