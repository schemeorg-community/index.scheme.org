package scmindex.core

import scala.annotation.tailrec

sealed trait Sexpr;
case class SexprBool(value: Boolean) extends Sexpr {
  override def toString: String = {
    if (value) "#t" else "#f"
  }
}
case class SexprSymbol(name: String) extends Sexpr {
  override def toString: String = name
}
case class SexprString(value: String) extends Sexpr {
  override def toString: String = value
}
case class SexprNumber(i: Int) extends Sexpr {
  override def toString: String = "" + i
}
case class SexprPair(car: Sexpr, cdr: Sexpr) extends Sexpr {
  override def toString: String = {
    val (lst, tail) = Sexpr.pairToList(this)
    val lstStr = lst.map(_.toString).mkString(" ")
    val tailStr = tail match {
      case SexprNull => ""
      case _ => " . " + tail.toString
    }
    "(" + lstStr + tailStr + ")"
  }
}

case object SexprNull extends Sexpr {
  override def toString: String = "()"
}

object Sexpr {
  def parseListToPairs(value: List[Sexpr]): Sexpr = {
    parseListToPairs(value, SexprNull)
  }

  def parseListToPairs(value: List[Sexpr], tail: Sexpr): Sexpr = {
    value match {
      case x :: xs => SexprPair(x, parseListToPairs(xs, tail))
      case Nil => tail
    }
  }

  def pairToList(value: SexprPair): (List[Sexpr], Sexpr) = {
    @tailrec
    def fold(value: SexprPair, lst: List[Sexpr]): (List[Sexpr], Sexpr) = {
      value match {
        case SexprPair(car, cdr: SexprPair) => fold(cdr, car +: lst)
        case SexprPair(car, cdr) => ((car +: lst).reverse, cdr)
      }
    }
    fold(value, List())
  }

  def sexprToProperList(value: Sexpr): Either[Exception, List[Sexpr]] = {
    value match {
      case SexprNull => Right(List())
      case pair: SexprPair => {
        val (lst, tail)  = pairToList(pair)
        if (tail == SexprNull)
          Right(lst)
        else
          Left(Exception("sexpr was dotted list, not a proper list"))
      }
      case _ => Left(Exception("sexpr not a proper list"))
    }
  }

  def alistToMap(sexpr: Sexpr): Either[Exception, Map[String, Sexpr]] = {
    sexprToProperList(sexpr).flatMap { items =>
      val entries = items.map {
        case SexprPair(SexprSymbol(key), value) => Right((key, value))
        case SexprPair(key, _) => Left(Exception(s"Key should be a symbol, was: ${key.toString}"))
        case e => Left(Exception(s"Entry should be a pair, was: ${e.toString}"))
      }
      entries.partitionMap(identity) match {
        case (Nil, items) => Right(Map.from(items))
        case (first :: _, _) => Left(Exception("Failed to parse alist to map", first))
      }
    }
  }
}
