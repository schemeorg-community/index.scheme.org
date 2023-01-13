package scmindex

import java.security.Signature

//TODO rename
case class IndexEntry(file: String, exclude: List[String])

sealed trait ParamType
sealed trait ReturnType

case class Predicate(identifier: String) extends ParamType, ReturnType
case object LiteralFalse extends ParamType, ReturnType
case object Unknown extends ParamType, ReturnType
case object Ellipsis extends ParamType, ReturnType
case class UnionParamType(types: List[ParamType]) extends ParamType
case class UnionReturnType(types: List[ReturnType]) extends ReturnType
case class Values(items: List[ReturnType]) extends ReturnType
case object Undefined extends ReturnType

case class Parameter(name: String, `type`: ParamType)

case class PatternAndType(pattern: Sexpr, `type`: Option[ReturnType])

sealed trait Signature
case class SigValue(predicate: String) extends Signature
case class SigLambda(params: List[Parameter], `return`: ReturnType) extends Signature
case class SigCaseLambda(cases: List[SigLambda]) extends Signature
case class SigSyntaxRules(keywords: List[String], rules: List[PatternAndType]) extends Signature

sealed trait SubSignature extends Signature
case class AList(car: Parameter, cdr: Parameter) extends SubSignature
case class SCMList(el: Parameter) extends SubSignature
case class SCMVector(el: Parameter) extends SubSignature
case class Patterns(patterns: List[Sexpr]) extends SubSignature

case class SubSigEntry(paramName: String, signature: SubSignature)

case class SCMIndexEntry(name: String, lib: String, signature: Signature, subsignatures: List[SubSigEntry], tags: List[String], description: String)

def parseSCMIndexEntries(lib: String, sexpr: Sexpr): Either[String, List[SCMIndexEntry]] = {
  sexprToProperList(sexpr)
    .toRight("type file should be a list")
    .flatMap { lst =>
      lst.partitionMap { e => parseSCMIndexEntry(lib, e)} match {
        case (Nil, values) => Right(values)
        case (errs, _) => Left(errs.mkString(", "))
      }
    }
}

def parseSCMIndexEntry(lib: String, sexpr: Sexpr): Either[String, SCMIndexEntry] = {
  alistToMap(sexpr) match {
    case Right(map) => {
      val name = map.get("name") match {
        case Some(SexprString(n)) => Right(n)
        case Some(_) => Left("Name should be a string")
        case _ => Left("Missing name")
      }
      val tags = map.get("tags") match {
        case Some(sexpr) => readListOfSymbols(sexpr, "tags should be a list of symbols")
        case None => Right(List())
      }
      val desc = map.get("desc") match {
        case Some(SexprString(desc)) => Right(desc)
        case Some(_) => Left("Description should be a string")
        case None => Right("")
      }
      val signature = map.get("signature") match {
        case Some(sexpr) => parseSignature(sexpr)
        case None => Left("Missing signature")
      }
      val subsigs = map.get("subsigs") match  {
        case Some(sexpr) => parseSubsigs(sexpr)
        case None => Right(List())
      }
      (name, signature, subsigs, tags, desc) match {
        case (Right(name), Right(sig), Right(subsigs), Right(tags), Right(desc)) => Right(SCMIndexEntry(name, lib, sig, subsigs, tags, desc))
        case _ => List(name, signature, subsigs, tags, desc).partitionMap(identity) match {
          case (errors, _) => Left(errors.mkString(", "))
        }
      }
    }
    case Left(err) => Left(err)
  }
}

def parseSignature(sexpr: Sexpr): Either[String, Signature] = {
  sexpr match {
    case SexprPair(SexprSymbol("lambda"), cdr) => parseLambdaSignature(cdr)
    case SexprPair(SexprSymbol("case-lambda"), cdr) => parseCaseLambdaSignature(cdr)
    case SexprPair(SexprSymbol("value"), cdr) => parseValueSignature(cdr)
    case SexprPair(SexprSymbol("syntax-rules"), cdr) => parseSyntaxRulesSignature(cdr)
    case _ => Left("Unrecognized signature")
  }
}

def parseLambdaSignature(sexpr: Sexpr) = {
  sexpr match {
    case SexprPair(args, SexprPair(returns, SexprNull)) => {
      (parseParams(args), parseReturn(returns)) match {
        case (Right(params), Right(returns)) => Right(SigLambda(params, returns))
        case (params, returns) => List(params, returns).partitionMap(identity) match {
          case (errors, _) => Left(errors.mkString(", "))
        }
      }
    }
    case _ => Left("Invalid lambda signature")
  }
}

def parseParams(sexpr: Sexpr): Either[String, List[Parameter]] = {
  def parseParam(sexpr: Sexpr): Either[String, Parameter] = {
    sexpr match {
      case SexprSymbol("...") => Right(Parameter("", Ellipsis))
      case SexprSymbol(name) => Right(Parameter(name, Unknown))
      case pair: SexprPair => {
        val (lst, tail) = pairToList(pair)
        lst match {
          case t :: SexprSymbol(name) :: Nil => {
            parseType(t) match {
              case Right(t) => Right(Parameter(name, t))
              case Left(err) => Left(err)
            }
          }
          case _ => Left("invalid parameter definition")
        }
      }
      case _ => Left("invalid parameter definition")
    }
  }
  sexprToProperList(sexpr) match {
    case Some(paramSexprs) => {
      paramSexprs.map(parseParam).partitionMap(identity) match {
        case (Nil, params) => Right(params)
        case (err, _) => Left(err.mkString(", "))
      }
    }
    case _ => Left("Param list must be a list")
  }
}

def parseType(sexpr: Sexpr): Either[String, ParamType] = {
  sexpr match {
    case SexprSymbol(name) => Right(Predicate(name))
    case SexprBool(false) => Right(LiteralFalse)
    case SexprPair(SexprSymbol("or"), cdr) => {
      cdr match {
        case pair: SexprPair => {
          val (sexprs, tail) = pairToList(pair)
          sexprs.map(parseType).partitionMap(identity) match {
            case (Nil, types) => Right(UnionParamType(types))
            case (err, _) => Left(err.mkString(", "))
          }
        }
        case _ => Left("Bad type definition")
      }
    }
    case _ => Left("Bad type definition")
  }
}

def parseReturn(sexpr: Sexpr): Either[String, ReturnType] = {
  sexpr match {
    case SexprSymbol("undefined") => Right(Undefined)
    case SexprSymbol("...") => Right(Ellipsis)
    case SexprSymbol(pred) => Right(Predicate(pred))
    case SexprBool(false) => Right(LiteralFalse)
    case SexprPair(SexprSymbol("values"), cdr: SexprPair) =>
      val (lst, tail) = pairToList(cdr)
      lst.map(parseReturn).partitionMap(identity) match {
        case (Nil, vals) => Right(Values(vals))
        case (errs, _) => Left(errs.mkString(", "))
      }
    case SexprPair(SexprSymbol("or"), cdr: SexprPair) =>
      val (lst, tail) = pairToList(cdr)
      lst.map(parseReturn).partitionMap(identity) match {
        case (Nil, vals) => Right(UnionReturnType(vals))
        case (errs, _) => Left(errs.mkString(", "))
      }
    case _ => Left("Unknown return format")
  }
}

def parseCaseLambdaSignature(sexpr: Sexpr) = {
  sexpr match {
    case pair: SexprPair => {
      val (sexprs, tail) = pairToList(pair)
      sexprs.map(parseLambdaSignature).partitionMap(identity) match {
        case (Nil, values) => Right(SigCaseLambda(values))
        case (errors, _) => Left(errors.mkString(", "))
      }
    }
    case _ => Left("case-lambda cases should be a list")
  }
}

def parseValueSignature(sexpr: Sexpr): Either[String, SigValue] = {
  sexpr match {
    case SexprPair(SexprSymbol(predicate), SexprNull) => Right(SigValue(predicate))
    case _ => Left("Unrecognized value signature")
  }
}

def parseSyntaxRulesSignature(sexpr: Sexpr) = {
  def readPatterns(sexpr: Sexpr): Either[String, List[PatternAndType]] = {
    sexprToProperList(sexpr)
      .toRight("syntax-rules bad pattern list")
      .flatMap{ lst =>
        lst.partitionMap { e => readPattern(e) } match {
          case (Nil, patterns) => Right(patterns)
          case (err, _) => Left(err.mkString(", "))
        }
      }
  }
  def readPattern(sexpr: Sexpr): Either[String, PatternAndType] = {
    sexprToProperList(sexpr)
      .toRight("syntax-rules bad pattern")
      .flatMap{
        case pat :: Nil => Right(PatternAndType(pat, None))
        case pat :: returnSexpr :: Nil => parseReturn(returnSexpr).map {returnType => PatternAndType(pat, Some(returnType)) }
        case any => Left(s"syntax-rules bad pattern: ${any}")
      }
  }
  for {
    sexprs <- { sexpr match {
      case SexprPair(keywordsSexpr, rulesSexpr) => Right((keywordsSexpr, rulesSexpr))
      case _ => Left("invalid syntax-rules signature")
    }}: Either[String, (Sexpr, Sexpr)]
    keywordsSexpr = sexprs._1
    rulesSexpr = sexprs._2
    keywords <- readListOfSymbols(keywordsSexpr, "keywords must be list of symbols")
    rules <- readPatterns(rulesSexpr)
  } yield {
    SigSyntaxRules(keywords, rules)
  }
}

def parseSubsigs(sexpr: Sexpr): Either[String, List[SubSigEntry]] = {
  //TODO
  Right(List())
}

def readListOfSymbols(sexpr: Sexpr, err: String): Either[String, List[String]] = {
  sexprToProperList(sexpr).flatMap { lst =>
    val maybeValues = lst.partitionMap {
      case SexprSymbol(name) => Right(name)
      case _ => Left("")
    } match {
      case (Nil, values) => Some(values)
      case _ => None
    }
    maybeValues
  }.toRight(err)
}

def loadSignatures(loader: SignatureLoader): Either[String, List[SCMIndexEntry]] = {
  def parseIndexEntry(sexpr: Sexpr): Either[String, IndexEntry] = {
    sexpr match {
      case SexprString(v) => Right(IndexEntry(v, List()))
      case _ => for {
        map <- alistToMap(sexpr)
        fileSexpr <- map.get("file").toRight("missing file field")
        file <- { fileSexpr match {
          case SexprString(file) => Right(file)
          case _ => Left("file field not a string")
        }}
        excludeSexpr = map.getOrElse("exclude", SexprNull)
        excludeSexprList <- sexprToProperList(excludeSexpr).toRight("exclude not a list")
        exclude <- excludeSexprList.partitionMap {
          case SexprSymbol(v) => Right(v)
          case _ => Left("Unexpected exclude identifier")
        } match {
          case (Nil, vals) => Right(vals)
          case (errs, _) => Left(errs.mkString(", "))
        }
      } yield IndexEntry(file, exclude)
    }
  }
  def readLibraryList(sexpr: Sexpr): Either[String, List[(String, IndexEntry)]] = {
    for {
      lst <- sexprToProperList(sexpr).toRight("Index should be a list")
      entries <- {
        lst
          .partitionMap {
            case SexprPair(lib, sexpr) => parseIndexEntry(sexpr).map { e => (lib.toString, e) }
            case o => Left(s"Wrong index format. Expected a pair, got: ${o}")
          } match {
            case (Nil, entries) => Right(entries)
            case (errors, _) => Left(errors.mkString(", "))
          }
      }
    } yield entries
  }

  for {
    indexSexpr <- loader.loadIndex()
    libs <- readLibraryList(indexSexpr)
    entries <- {
      libs
        .partitionMap { e =>
          val (libName, meta) = e
          for {
            libSexpr <- loader.loadLibrary(meta.file)
            entries <- parseSCMIndexEntries(libName, libSexpr).map {
              entries => entries.filter { e => !meta.exclude.contains(e.name) }
            }
          } yield entries
        } match {
          case (Nil, values) => Right(values.flatten)
          case (errors, _) => Left(errors.mkString(", "))
        }
    }
  } yield entries

}