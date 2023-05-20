package scmindex.core

object SexprWriter {

  def stringifyPair(pair: SexprPair, toStr: Sexpr => String) = {
    val (lst, tail) = Sexpr.pairToList(pair)
    val lstStr = lst.map(toStr(_)).mkString(" ")
    val tailStr = tail match {
      case SexprNull => ""
      case _ => " . " + toStr(tail)
    }
    "(" + lstStr + tailStr + ")"
  }

  def writeString(value: String): String = {
    "\"" +
    value
      .replace("\\", "\\\\") 
      .replace("\n", "\\\\n")
      .replace("\r", "\\\\r")
      .replace("\t", "\\\\t")
      + "\""
  }

  def writeSymbol(name: String): String = {
    if (name.contains("|")) {
      "|" +
      name
        .replace("\\", "\\\\")
        .replace("|", "\\|")
      + "|"
    } else name
  }

  def display(sexpr: Sexpr): String = {
    sexpr match {
      case SexprBool(value) => if value then "#t" else "#f"
      case SexprNull => "()"
      case SexprNumber(i) => "" + i
      case pair: SexprPair => stringifyPair(pair, display)
      case SexprSymbol(name) => name
      case SexprString(value) => value
    }
  }

  def write(sexpr: Sexpr): String = {
    sexpr match {
      case SexprString(value) => writeString(value)
      case SexprSymbol(name) => writeSymbol(name)
      case pair: SexprPair => stringifyPair(pair, write)
      case _ => display(sexpr)
    }
  }

}
