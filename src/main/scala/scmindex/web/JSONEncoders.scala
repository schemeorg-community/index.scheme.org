package scmindex.web

import scmindex.core.*
import scmindex.core.Sexpr.*
import io.circe.*
import io.circe.syntax.*
import io.circe.generic.semiauto.*
import io.circe.generic.auto.*

object JSONEncoders {

  given Encoder[SCMIndexEntry] with  {

    def encodeReturn(t: ReturnType): Json = {
      t match {
        case Values(items) => Json.obj(("kind", "values".asJson), ("items", Json.arr(items.map(encodeReturn):_*)))
        case UnionReturnType(items) => Json.obj(("kind", "or".asJson), ("items", Json.arr(items.map(encodeReturn):_*)))
        case LiteralFalse => Json.obj(("kind", "return".asJson), ("type", "#f".asJson))
        case Predicate(p) => Json.obj(("kind", "return".asJson), ("type", p.asJson))
        case Ellipsis => Json.obj(("kind", "return".asJson), ("type", "...".asJson))
        case Unknown => Json.obj(("kind", "return".asJson), ("type", "*".asJson))
        case Undefined => Json.obj(("kind", "return".asJson), ("type", "undefined".asJson))
      }
    }

    def flattenParamType(paramType: ParamType): List[String] = {
      paramType match {
        case LiteralFalse => List("#f")
        case Predicate(p) => List(p)
        case UnionParamType(types) => types.flatMap(flattenParamType)
        case _ => List()
      }
    }

    def encodeParam(p: Parameter): Json = {
      Json.obj(
        ("name", p.name.asJson),
        ("types", Json.arr(flattenParamType(p.`type`).map(_.asJson):_*)) )
    }

    def encodeLambdas(lambda: List[SigLambda]): Json = {
      Json.obj(
        ("type", "function".asJson),
        ("variants", Json.arr(lambda.map{ l =>
          Json.obj(("params", Json.arr(l.params.map(encodeParam):_*)), ("return", encodeReturn(l.`return`)))
        }:_*)))
    }

    def encodeSignature(signature: Signature): Json = {
      signature match {
        case l: SigLambda => encodeLambdas(List(l))
        case SigCaseLambda(cases) => encodeLambdas(cases)
        case SigValue(predicate) => Json.obj(("type", "value".asJson), ("value", encodeReturn(Predicate(predicate))))
        case SigSyntaxRules(keywords, rules) => Json.obj(
          ("type", "syntax".asJson),
          ("literals", Json.arr(keywords.map(_.asJson):_*)),
          ("patterns", Json.arr(rules.map{ rule =>
            Json.obj(
              ("pattern", rule.pattern.toString.asJson),
              ("type", rule.`type` match {
                case Some(ret) => encodeReturn(ret)
                case None => Json.Null
              }))
          }:_*)))
      }
    }

    def patternToString(pattern: Sexpr): String = {
      pattern match {
        case SexprPair(SexprSymbol("_append"), rest) => {
          sexprToProperList(rest) match {
            case Right(lst) => lst.map(_.toString).mkString("")
            case Left(_) => rest.toString
          }
        }
        case _ => pattern.toString
      }
    }

    def encodeSubSignature(sub: SubSignature): Json = {
      sub match {
        case Patterns(patterns) => Json.obj(("type", "pattern".asJson), ("patterns", patterns.map(patternToString).asJson))
        case AList(car, cdr) => Json.obj(("type", "alist".asJson), ("car", encodeParam(car)), ("cdr", encodeParam(cdr)))
        case SCMList(t) => Json.obj(("type", "list".asJson), ("element", encodeParam(t)))
        case SCMVector(t) => Json.obj(("type", "vector".asJson), ("element", encodeParam(t)))
        case s: Signature => encodeSignature(s)
      }
    }

    def encodeSubSignatureEntry(subSigEntry: SubSigEntry): Json = {
      Json.obj(
        ("name", Json.fromString(subSigEntry.paramName)),
        ("signature", encodeSubSignature(subSigEntry.signature)))
    }

    def encodeSCMIndexEntrySingle(a: SCMIndexEntrySingle): Json = {
      Json.obj(
        ("kind", "single".asJson),
        ("lib", Json.fromString(a.lib)),
        ("name", Json.fromString(a.name)),
        ("description", Json.fromString(a.description)),
        ("signature", encodeSignature(a.signature)),
        ("subsignatures", Json.arr(a.subsignatures.map(encodeSubSignatureEntry):_*)),
        ("tags", Json.arr(a.tags.map(Json.fromString(_)):_*))
      )
    }

    def encodeSCMIndexEntryGroup(a: SCMIndexEntryGroup): Json = {
      Json.obj(
        ("kind", "group".asJson),
        ("lib", Json.fromString(a.lib)),
        ("description", Json.fromString(a.description)),
        ("entries", Json.arr(a.entries.map(encodeSCMIndexEntrySingle):_*))
      )
    }

    override def apply(a: SCMIndexEntry): Json = {
      a match {
        case e: SCMIndexEntrySingle => encodeSCMIndexEntrySingle(e)
        case e: SCMIndexEntryGroup => encodeSCMIndexEntryGroup(e)
      }
    }
  }

}
