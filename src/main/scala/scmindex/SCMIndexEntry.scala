package scmindex

import cats.data.EitherT
import cats.effect.IO
import cats.implicits.*
import scmindex.Sexpr.sexprToProperList

import java.security.Signature
import org.slf4j.LoggerFactory

case class ContentFile(file: String, exclude: List[String])

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
sealed trait SubSignature

case class SigValue(predicate: String) extends Signature, SubSignature
case class SigLambda(params: List[Parameter], `return`: ReturnType) extends Signature, SubSignature
case class SigCaseLambda(cases: List[SigLambda]) extends Signature
case class SigSyntaxRules(keywords: List[String], rules: List[PatternAndType]) extends Signature

case class AList(car: Parameter, cdr: Parameter) extends SubSignature
case class SCMList(el: Parameter) extends SubSignature
case class SCMVector(el: Parameter) extends SubSignature
case class Patterns(patterns: List[Sexpr]) extends SubSignature

case class SubSigEntry(paramName: String, signature: SubSignature)

sealed trait SCMIndexEntry
case class SCMIndexEntrySingle(name: String, lib: String, signature: Signature, subsignatures: List[SubSigEntry], tags: List[String], description: String) extends SCMIndexEntry
case class SCMIndexEntryGroup(lib: String, entries: List[SCMIndexEntrySingle], description: String) extends SCMIndexEntry

object SCMIndexEntry {

  def log = LoggerFactory.getLogger("scmindexentry")

  def paramNamesSingle(e: SCMIndexEntrySingle): List[String] = {
    def singleParamNames(parameter: Parameter): List[String] = {
      List(parameter.name)
    }
    def paramNamesInLambda(l: SigLambda): List[String] = {
      l.params.flatMap(singleParamNames)
    }
    e.signature match {
      case SigCaseLambda(cases) => cases.flatMap(paramNamesInLambda)
      case l: SigLambda => paramNamesInLambda(l)
      case _ => List()
    }
  }

  def paramNames(e: SCMIndexEntry): List[String] = {
    e match {
      case s: SCMIndexEntrySingle => paramNamesSingle(s)
      case SCMIndexEntryGroup(_, entries, _) => entries.flatMap{ paramNamesSingle(_) }
    }
  }

  def paramTypesSingle(e: SCMIndexEntrySingle): List[String] = {
    def singleParamType(p: ParamType): List[String] = {
      p match {
        case Predicate(identifier) => List(identifier)
        case UnionParamType(types) => types.flatMap(singleParamType)
        case _ => List()
      }
    }
    def paramTypesInLambda(l: SigLambda): List[String] = {
      l.params.map(_.`type`).flatMap(singleParamType)
    }
    val typesInSig = e.signature match {
      case SigCaseLambda(cases) => cases.flatMap(paramTypesInLambda)
      case l: SigLambda => paramTypesInLambda(l)
      case _ => List()
    }
    val typesInSubtypes = e.subsignatures.flatMap {
      case SubSigEntry(paramName, SigValue(predicate)) => 
        if (paramName != "return")
          List(predicate)
        else
          List()
      case _ => List()
    }
    typesInSig ++ typesInSubtypes
  }

  def paramTypes(e: SCMIndexEntry): List[String] = {
    e match {
      case s: SCMIndexEntrySingle => paramTypesSingle(s)
      case SCMIndexEntryGroup(_, entries, _) => entries.flatMap{ paramTypesSingle(_) }
    }
  }

  def returnTypesSingle(e: SCMIndexEntrySingle): List[String] = {
    def returnType(r: ReturnType): List[String] = {
      r match {
        case Predicate(p) => List(p)
        case Values(v) => v.flatMap(returnType)
        case UnionReturnType(v) => v.flatMap(returnType)
        case _ => List()
      }
    }
    def returnTypesInLambda(l: SigLambda): List[String] = returnType(l.`return`)
    e.signature match {
      case SigCaseLambda(cases) => cases.flatMap(returnTypesInLambda)
      case l: SigLambda => returnTypesInLambda(l)
      case SigSyntaxRules(keywords, patterns) => patterns.flatMap {
        case PatternAndType(_, Some(ret)) => returnType(ret)
        case _ => List()
      }
      case _ => List()
    }
  }

  def returnTypes(e: SCMIndexEntry): List[String] = {
    e match {
      case s: SCMIndexEntrySingle => returnTypesSingle(s)
      case SCMIndexEntryGroup(_, entries, _) => entries.flatMap{ returnTypesSingle(_) }
    }
  }

  def parseSCMIndexEntries(lib: String, sexpr: Sexpr): Either[Exception, List[SCMIndexEntry]] = {
    Sexpr.sexprToProperList(sexpr)
      .flatMap { lst =>
        lst.partitionMap { e => parseSCMIndexEntry(lib, e)} match {
          case (Nil, values) => Right(values)
          case (err :: _, _) => Left(Exception("Failed to parse scm index entries", err))
        }
      }
  }

  def parseSCMIndexEntry(lib: String, sexpr: Sexpr): Either[Exception, SCMIndexEntry] = {
    Sexpr.alistToMap(sexpr).flatMap { map =>
      map.get("group") match {
        case Some(group) => for {
          groupContentSexprList <- Sexpr.sexprToProperList(group)
          desc <- map.get("desc") match {
            case Some(SexprString(desc)) => Right(desc)
            case Some(_) => Left(Exception("Description should be a string"))
            case None => {
              log.warn("Missing description for group {} in library {}", group, lib)
              Right("")
            }
          }
          groupContent <- groupContentSexprList.partitionMap{ el => parseSCMIndexEntrySingle(lib, el) } match {
            case (Nil, items) => Right(items)
            case (err :: _, _) => Left(err)
          }
        } yield SCMIndexEntryGroup(lib, groupContent, desc)
        case None => {
          val maybeEntry = parseSCMIndexEntrySingle(lib, sexpr)
          maybeEntry match {
            case Right(e) => {
              if (e.description == "") {
                log.warn("Missing description for {} in library {}", e.name, lib)
              }
            }
            case _ => null
          }
          maybeEntry
        }
      }
    }
  }

  def parseSCMIndexEntrySingle(lib: String, sexpr: Sexpr): Either[Exception, SCMIndexEntrySingle] = {
    Sexpr.alistToMap(sexpr).flatMap { map =>
      for {
        name <- map.get("name") match {
          case Some(SexprString(n)) => Right(n)
          case Some(_) => Left(Exception("Name should be a string"))
          case _ => Left(Exception("Missing name"))
        }
        tags <- map.get("tags") match {
          case Some(sexpr) => readListOfSymbols(sexpr, "tags should be a list of symbols")
          case None => Right(List())
        }
        desc <- map.get("desc") match {
          case Some(SexprString(desc)) => Right(desc)
          case Some(_) => Left(Exception("Description should be a string"))
          case None => Right("")
        }
        signature <- map.get("signature") match {
          case Some(sexpr) => parseSignature(sexpr)
          case None => Left(Exception("Missing signature"))
        }
        subsigs <- map.get("subsigs") match  {
          case Some(sexpr) => parseSubsigs(sexpr)
          case None => Right(List())
        }
      } yield (SCMIndexEntrySingle(name, lib, signature, subsigs, tags, desc))
    }
  }

  def parseSignature(sexpr: Sexpr): Either[Exception, Signature] = {
    sexpr match {
      case SexprPair(SexprSymbol("lambda"), cdr) => parseLambdaSignature(cdr)
      case SexprPair(SexprSymbol("case-lambda"), cdr) => parseCaseLambdaSignature(cdr)
      case SexprPair(SexprSymbol("value"), cdr) => parseValueSignature(cdr)
      case SexprPair(SexprSymbol("syntax-rules"), cdr) => parseSyntaxRulesSignature(cdr)
      case _ => Left(Exception(s"Unrecognized signature: ${sexpr}"))
    }
  }

  def parseLambdaSignature(sexpr: Sexpr) = {
    sexpr match {
      case SexprPair(args, SexprPair(returns, SexprNull)) => {
        for {
          params <- parseParams(args)
          returns <- parseReturn(returns)
        } yield SigLambda(params, returns)
      }
      case _ => Left(Exception(s"Invalid lambda signature, expected a pair: ${sexpr}"))
    }
  }
  def parseParam(sexpr: Sexpr): Either[Exception, Parameter] = {
    sexpr match {
      case SexprSymbol("...") => Right(Parameter("...", Ellipsis))
      case SexprSymbol(name) => Right(Parameter(name, Unknown))
      case pair: SexprPair => {
        Sexpr.sexprToProperList(pair).flatMap {
          case t :: SexprSymbol(name) :: Nil => {
            parseType(t) match {
              case Right(t) => Right(Parameter(name, t))
              case Left(err) => Left(err)
            }
          }
          case _ => Left(Exception(s"invalid parameter definition: ${sexpr}"))
        }
      }
      case _ => Left(Exception(s"invalid parameter definition: ${sexpr}"))
    }
  }
  def parseParams(sexpr: Sexpr): Either[Exception, List[Parameter]] = {

    Sexpr.sexprToProperList(sexpr).flatMap { paramSexprs =>
      paramSexprs.map(parseParam).partitionMap(identity) match {
        case (Nil, params) => Right(params)
        case (err :: _, _) => Left(Exception("Failed to parse params in lambda signatuer", err))
      }
    }
  }

  def parseType(sexpr: Sexpr): Either[Exception, ParamType] = {
    sexpr match {
      case SexprSymbol(name) => Right(Predicate(name))
      case SexprBool(false) => Right(LiteralFalse)
      case SexprPair(SexprSymbol("or"), cdr) => {
        Sexpr.sexprToProperList(cdr).flatMap { sexprs =>
          sexprs.map(parseType).partitionMap(identity) match {
            case (Nil, types) => Right(UnionParamType(types))
            case (err :: _, _) => Left(Exception("Failed parsing type", err))
          }
        }
      }
      case _ => Left(Exception("Bad type definition"))
    }
  }

  def parseReturn(sexpr: Sexpr): Either[Exception, ReturnType] = {
    sexpr match {
      case SexprSymbol("undefined") => Right(Undefined)
      case SexprSymbol("*") => Right(Unknown)
      case SexprSymbol("...") => Right(Ellipsis)
      case SexprSymbol(pred) => Right(Predicate(pred))
      case SexprBool(false) => Right(LiteralFalse)
      case SexprPair(SexprSymbol("values"), cdr: SexprPair) =>
        Sexpr.sexprToProperList(cdr).flatMap { lst =>
          lst.map(parseReturn).partitionMap(identity) match {
            case (Nil, vals) => Right(Values(vals))
            case (err :: _, _) => Left(Exception("Failed to parse `values` clause for return", err))
          }
        }
      case SexprPair(SexprSymbol("or"), cdr: SexprPair) =>
        Sexpr.sexprToProperList(cdr).flatMap { lst =>
          lst.map(parseReturn).partitionMap(identity) match {
            case (Nil, vals) => Right(UnionReturnType(vals))
            case (err :: _, _) => Left(Exception("Failed to parse `or` clause for return", err))
          }
        }
      case _ => Left(Exception("Unknown return format"))
    }
  }

  def parseCaseLambdaSignature(sexpr: Sexpr) = {
    Sexpr.sexprToProperList(sexpr).flatMap { sexprs =>
      sexprs.map(parseLambdaSignature).partitionMap(identity) match {
        case (Nil, values) => Right(SigCaseLambda(values))
        case (err :: _, _) => Left(Exception("Failed to parse case-lambda signature", err))
      }
    }
  }

  def parseValueSignature(sexpr: Sexpr): Either[Exception, SigValue] = {
    sexpr match {
      case SexprPair(SexprSymbol(predicate), SexprNull) => Right(SigValue(predicate))
      case _ => Left(Exception("Bad value signature"))
    }
  }

  def parseSyntaxRulesSignature(sexpr: Sexpr) = {
    def readPatterns(sexpr: Sexpr): Either[Exception, List[PatternAndType]] = {
      Sexpr.sexprToProperList(sexpr)
        .flatMap{ lst =>
          lst.partitionMap { e => readPattern(e) } match {
            case (Nil, patterns) => Right(patterns)
            case (err :: _, _) => Left(Exception("Failed to read syntax-rules patterns", err))
          }
        }
    }
    def readPattern(sexpr: Sexpr): Either[Exception, PatternAndType] = {
      Sexpr.sexprToProperList(sexpr)
        .flatMap{
          case pat :: Nil => Right(PatternAndType(pat, None))
          case pat :: returnSexpr :: Nil => parseReturn(returnSexpr).map {returnType => PatternAndType(pat, Some(returnType)) }
          case any => Left(Exception(s"syntax-rules bad pattern: ${any}"))
        }
    }
    for {
      sexprs <- { sexpr match {
        case SexprPair(keywordsSexpr, rulesSexpr) => Right((keywordsSexpr, rulesSexpr))
        case _ => Left(Exception("invalid syntax-rules signature"))
      }}
      keywordsSexpr = sexprs._1
      rulesSexpr = sexprs._2
      keywords <- readListOfSymbols(keywordsSexpr, "keywords must be list of symbols")
      rules <- readPatterns(rulesSexpr)
    } yield {
      SigSyntaxRules(keywords, rules)
    }
  }

  def parseSubsigs(sexpr: Sexpr): Either[Exception, List[SubSigEntry]] = {
    sexprToProperList(sexpr).flatMap { lst =>
      lst.partitionMap(parseSubsig) match {
        case (Nil, values) => Right(values)
        case (err :: _, _) => Left(err)
      }
    }
  }

  def parseSubsig(sexpr: Sexpr): Either[Exception, SubSigEntry] = {
    sexprToProperList(sexpr).flatMap {
      case SexprSymbol(name) :: s :: Nil => doParseSubsig(name, s)
      case s => Left(Exception("Wrong subsignature format: " + s))
    }
  }

  def doParseSubsig(name: String, sexpr: Sexpr): Either[Exception, SubSigEntry] = {
    (sexpr match {
      case SexprPair(SexprSymbol("lambda"), cdr) => parseLambdaSignature(cdr)
      case SexprPair(SexprSymbol("value"), cdr) => parseValueSignature(cdr)
      case SexprPair(SexprSymbol("list"), SexprPair(param, SexprNull)) => parseParam(param).map(p => SCMList(p))
      case SexprPair(SexprSymbol("vector"), SexprPair(param, SexprNull)) => parseParam(param).map(p => SCMVector(p))
      case SexprPair(SexprSymbol("alist"), SexprPair(car, SexprPair(cdr, SexprNull))) =>
        parseParam(car).flatMap { parsedCar =>
          parseParam(cdr).map { parsedCdr =>
            AList(parsedCar, parsedCdr)
          }
        }
      case SexprPair(SexprSymbol("pattern"), cdr) => sexprToProperList(cdr).map { lst =>
        Patterns(lst)
      }
      case _ => Left(Exception(s"Unrecognized subsignature pattern: ${sexpr}"))
    }).map{ l => SubSigEntry(name, l) }
  }

  def readListOfSymbols(sexpr: Sexpr, err: String): Either[Exception, List[String]] = {
    Sexpr.sexprToProperList(sexpr).flatMap { lst =>
      val maybeValues = lst.partitionMap {
        case SexprSymbol(name) => Right(name)
        case sexpr => Left(Exception(s"expected a symbol, got: ${sexpr}"))
      } match {
        case (Nil, values) => Right(values)
        case (err :: _, _) => Left(err)
      }
      maybeValues
    }
  }

  def loadSignatures[T : SignatureLoader](loader: T): IO[Either[Exception, List[SCMIndexEntry]]] = {
    def parseIndexEntry(sexpr: Sexpr): Either[Exception, ContentFile] = {
      sexpr match {
        case SexprString(v) => Right(ContentFile(v, List()))
        case _ => for {
          map <- Sexpr.alistToMap(sexpr)
          fileSexpr <- map.get("file").toRight(Exception("missing file field"))
          file <- { fileSexpr match {
            case SexprString(file) => Right(file)
            case _ => Left(Exception("file field not a string"))
          }}
          excludeSexpr = map.getOrElse("exclude", SexprNull)
          excludeSexprList <- Sexpr.sexprToProperList(excludeSexpr)
          exclude <- excludeSexprList.partitionMap {
            case SexprSymbol(v) => Right(v)
            case _ => Left(Exception("Unexpected exclude identifier"))
          } match {
            case (Nil, vals) => Right(vals)
            case (err :: _, _) => Left(Exception("Failed to load signature", err))
          }
        } yield ContentFile(file, exclude)
      }
    }
    def readLibraryList(sexpr: Sexpr): Either[Exception, List[(String, ContentFile)]] = {
      for {
        lst <- Sexpr.sexprToProperList(sexpr)
        entries <- {
          lst
            .partitionMap {
              case SexprPair(lib, sexpr) => parseIndexEntry(sexpr).map { e => (lib.toString, e) }
              case o => Left(Exception(s"Wrong index format. Expected a pair, got: ${o}"))
            } match {
            case (Nil, entries) => Right(entries)
            case (err :: _, _) => Left(Exception("Failed to read library list", err))
          }
        }
      } yield entries
    }

    val result = for {
      indexSexpr <- EitherT(loader.loadIndex())
      libs <- EitherT.fromEither(readLibraryList(indexSexpr))
      entries <- {
        val stuff = libs.map { e =>
          val (libName, meta) = e
          for {
            libSexpr <- EitherT(loader.loadLibrary(meta.file))
            entries <- EitherT.fromEither(parseSCMIndexEntries(libName, libSexpr).map {
              entries => entries
                .map {
                  case e: SCMIndexEntrySingle => e
                  case SCMIndexEntryGroup(lib, lst, desc) => SCMIndexEntryGroup(lib, lst.filter{e => !meta.exclude.contains(e.name) }, desc)
                }
                .filter {
                  case e: SCMIndexEntrySingle => !meta.exclude.contains(e.name)
                  case _ => true
                }
            })
          } yield entries
        }
        stuff.sequence.map { els => els.flatMap (identity) }
      }
    } yield entries
    result.value
  }
}
