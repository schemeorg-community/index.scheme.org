package scmindex

import scala.annotation.tailrec
//import scala.util.parsing.combinator.*

sealed trait Sexpr;
case class SexprBool(value: Boolean) extends Sexpr {
  override def toString: String = {
    if (value) "bool:#t" else "bool:#f"
  }
}
case class SexprSymbol(name: String) extends Sexpr {
  override def toString: String = "symbol:" + name
}
case class SexprString(value: String) extends Sexpr {
  override def toString: String = s"string:\"$value\""
}
case class SexprNumber(i: Int) extends Sexpr {
  override def toString: String = "number:" + i
}
case class SexprPair(car: Sexpr, cdr: Sexpr) extends Sexpr {
  override def toString: String = {
    val (lst, tail) = pairToList(this)
    val lstStr = lst.map(_.toString).mkString(" ")
    val tailStr = tail match {
      case SexprNull => ""
      case _ => " . " + tail.toString
    }
    "pair:(" + lstStr + tailStr + ")"
  }
}

case object SexprNull extends Sexpr {
  override def toString: String = "()"
}

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

def sexprToProperList(value: Sexpr): Option[List[Sexpr]] = {
  value match {
    case SexprNull => Some(List())
    case pair: SexprPair => {
      val (lst, tail)  = pairToList(pair)
      if (tail == SexprNull)
        Some(lst)
      else
        None
    }
    case _ => None
  }
}

/*
private class SimpleParser extends JavaTokenParsers {
  def `comment`: Parser[Any] = """;.*\n""".r
  def `false`: Parser[Sexpr] = """(#f|#false)""".r ^^ { x => SexprBool(false) }
  def `true`: Parser[Sexpr] = """(#t|#true)""".r ^^ { x => SexprBool(true) }
  def `string`: Parser[Sexpr] = ("\""+"""([^"\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4}|\n)*+"""+"\"").r ^^ { x => SexprString(x.substring(1, x.length - 1)) }
  def `number`: Parser[Sexpr] = wholeNumber ^^ { x => SexprNumber(x.toInt) }
  def `symbol`: Parser[Sexpr] = """([^ ().;"]+)|(\.[^ ()";]+)""".r ^^ {x => SexprSymbol(x)}
  def `list`: Parser[Sexpr] = """\(""".r ~ sexpr.* ~ """\)""".r ^^ { case _ ~ exprs ~ _ => parseListToPairs(exprs) }
  def `dottedlist`: Parser[Sexpr] = """\(""".r ~ sexpr.* ~ """\.""".r ~ sexpr ~ """\)""".r ^^ { case _ ~ exprs ~ _ ~ expr ~ _ => parseListToPairs(exprs, expr) }
  def `null`: Parser[Sexpr] = """\(""".r ~ """\)""".r ^^ { x => SexprNull }
  def `sexprNoComment`: Parser[Sexpr] = `false` | `true` | `string` | `number` | `symbol` | `dottedlist` | `list` | `null`
  def `sexpr`: Parser[Sexpr] = comment.* ~ `sexprNoComment` ~ comment.* ^^ { case _ ~ sexpr ~ _ => sexpr}


  def read(input: String): Either[String, Sexpr] = {
    val result = parse(sexpr, input);
    result match {
      case Success(matched, _) => Right(matched);
      case fail => Left(fail.toString)
    }
  }
}

def read(input: String): Either[String, Sexpr] = {
  val parser = SimpleParser();
  parser.read(input)
}
*/

def alistToMap(sexpr: Sexpr): Either[String, Map[String, Sexpr]] = {
  sexprToProperList(sexpr) match {
    case Some(items) => {
      val entries = items.map {
        case SexprPair(SexprSymbol(key), value) => Right((key, value))
        case SexprPair(key, _) => Left(s"Key should be a symbol, was: ${key.toString}")
        case e => Left(s"Entry should be a pair, was: ${e.toString}")
      }
      entries.partitionMap(identity) match {
        case (Nil, items) => Right(Map.from(items))
        case (err, _) => Left(err.mkString(", "))
      }
    }
    case _ => Left("Expected a list of entries")
  }
}