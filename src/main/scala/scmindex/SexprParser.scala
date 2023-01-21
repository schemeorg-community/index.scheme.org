package scmindex

import cats.effect.IO

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.tailrec

sealed trait Lexeme
case object Open extends Lexeme
case object Close extends Lexeme
case object Dot extends Lexeme
case class StringLexeme(content: String) extends Lexeme
case class IntegerLexeme(content: Int) extends Lexeme
case class SymbolLexeme(content: String) extends Lexeme

class Lexer(val source: String) {
  var pos: Int = 0;
  def skipWhitespace(): Unit = {
    while (pos < source.length && (source.charAt(pos) == ' ' || source.charAt(pos) == '\n' || source.charAt(pos) == '\r')) {
      pos += 1
    }
  }
  def skipComment(): Unit = {
    if (pos < source.length && source.charAt(pos) == ';') {
      while (pos < source.length && source.charAt(pos) != '\n') {
        pos += 1
      }
      pos += 1
    }
  }
  def skipWhitespaceAndComments(): Unit = {
    @tailrec
    def loop(startPos: Int): Unit = {
      skipWhitespace()
      skipComment()
      if (startPos != pos)
        loop(pos)
    }
    loop(pos)
  }
  def readStringLiteral(): Option[StringLexeme] = {
    @tailrec
    def findEnd(from: Int): Option[Int] = {
      val end = source.indexOf('"', from)
      if (end == -1) {
        None
      } else if (source.charAt(end - 1) == '\\') {
        findEnd(end + 1)
      } else {
        Some(end)
      }
    }
    findEnd(pos + 1) match {
      case Some(end) => {
        val l = StringLexeme(source.substring(pos + 1, end))
        pos = end + 1
        Some(l)
      }
      case _ => None
    }
  }
  def readIntLiteral(): Option[IntegerLexeme] = {
    @tailrec
    def findEnd(p: Int): Int = {
      if (p >= source.length || !Character.isDigit(source.charAt(p))) {
        p
      } else {
        findEnd(p + 1)
      }
    }
    val newPos = findEnd(pos)
    val l = IntegerLexeme(source.substring(pos, newPos).toInt)
    pos = newPos
    Some(l)
  }
  def readSymbol(): SymbolLexeme = {
    def break = List(' ', '\n', '\r', ';', ')', '(')
    def findEnd(): Int = {
      var p = pos;
      while (p < source.length && !break.contains(source.charAt(p))) {
        p += 1
      }
      p
    }
    val end = findEnd()
    val l = SymbolLexeme(source.substring(pos, end))
    pos = end
    l
  }
  def next(): Option[Lexeme] = {
    skipWhitespaceAndComments()
    def break = List(' ', '\n', '\r', ';', ')', '(')
    if (pos >= source.length) {
      None
    } else if (source.charAt(pos) == '(') {
      pos += 1
      Some(Open)
    } else if (source.charAt(pos) == ')') {
      pos += 1
      Some(Close)
    } else if (source.charAt(pos) == '.' && break.contains(source.charAt(pos + 1))) {
      pos += 1
      Some(Dot)
    } else if (source.charAt(pos) == '"') {
      readStringLiteral()
    } else if (Character.isDigit(source.charAt(pos))) {
      readIntLiteral()
    } else {
      Some(readSymbol())
    }
  }
}

object SexprParser {

  def streamLexer(source: String): LazyList[Lexeme] = {
    LazyList.unfold(new Lexer(source)) { lexer =>
      lexer.next().map { (_, lexer) }
    }
  }

  def readFromFile(file: String): IO[Either[Exception, Sexpr]] = IO {
    val string = Files.readString(Path.of(file), StandardCharsets.UTF_8);
    read(string)
  }

  def read(source: String): Either[Exception, Sexpr] = {
    read(streamLexer(source)).map { _._1 }
  }

  def read(lexemes: LazyList[Lexeme]): Either[Exception, (Sexpr, LazyList[Lexeme])] = {
    lexemes match {
      case Open #:: rest => readList(rest)
      case StringLexeme(content) #:: rest => Right(SexprString(content), rest)
      case IntegerLexeme(content) #:: rest => Right(SexprNumber(content), rest)
      case SymbolLexeme("#t") #:: rest => Right(SexprBool(true), rest)
      case SymbolLexeme("#f") #:: rest => Right(SexprBool(false), rest)
      case SymbolLexeme(content) #:: rest => Right(SexprSymbol(content), rest)
      case _ => Left(Exception(s"Failed to parse sexpr: ${lexemes}"))
    }
  }

  def readList(lexemes: LazyList[Lexeme]): Either[Exception, (Sexpr, LazyList[Lexeme])] = {
    @tailrec
    def readInside(lexemes: LazyList[Lexeme], content: List[Sexpr]): Either[Exception, (Sexpr, LazyList[Lexeme])] = {
      lexemes match {
        case Close #:: rest => {
          Right(Sexpr.parseListToPairs(content.reverse), rest)
        }
        case Dot #:: rest => {
          read(rest).flatMap {
            case (sexpr, Close #:: rest) => Right(Sexpr.parseListToPairs(content.reverse, sexpr), rest)
            case _ => Left(Exception("Bad dotted list"))
          }
        }
        case _ => read(lexemes) match {
          case Right((sexpr, lexemes)) => readInside(lexemes, sexpr +: content)
          case Left(err) => Left(err)
        }
      }
    }
    lexemes match {
      case Close #:: rest => Right(SexprNull, rest)
      case Dot #:: rest => Left(Exception("Unexpected period"))
      case _ => readInside(lexemes, List())
    }
  }
}