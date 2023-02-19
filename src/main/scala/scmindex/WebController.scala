package scmindex

import cats.effect.*
import com.comcast.ip4s.*
import io.circe.Encoder
import org.http4s.{HttpRoutes, QueryParamDecoder, Response, Status}
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.ember.server.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import cats.effect.*
import io.circe.*
import org.http4s.circe.CirceEntityEncoder.*
import cats.data.EitherT
import org.slf4j.LoggerFactory
import io.circe.*
import io.circe.generic.semiauto.*
import scmindex.Sexpr.sexprToProperList

object WebController {

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
                case Some(Predicate(p)) => p.asJson
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
        case _ => encodeSignature(sub)
      }
    }

    def encodeSubSignatureEntry(subSigEntry: SubSigEntry): Json = {
      Json.obj(
        ("name", Json.fromString(subSigEntry.paramName)),
        ("signature", encodeSubSignature(subSigEntry.signature)))
    }

    override def apply(a: SCMIndexEntry): Json = {
      Json.obj(
        ("lib", Json.fromString(a.lib)),
        ("name", Json.fromString(a.name)),
        ("description", Json.fromString(a.description)),
        ("signature", encodeSignature(a.signature)),
        ("subsignatures", Json.arr(a.subsignatures.map(encodeSubSignatureEntry):_*)),
        ("tags", Json.arr(a.tags.map(Json.fromString(_)):_*))
      )
    }
  }


  def log = LoggerFactory.getLogger("routes")

  object QueryQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("query")
  object StartQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Int]("start")
  object RowsQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Int]("rows")
  object LibsQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("lib")
  object ParamQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("param")
  object ReturnQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("return")
  object TagQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("tag")
  object FacetQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Boolean]("facet")

  def makeRoutes[T : Indexer](model: Model[T]) = HttpRoutes.of[IO] {
    case GET -> Root / "rest" / "filterset" => Ok(model.filtersets.map(f => Map("code" -> f.code, "name" -> f.name)).asJson)
    case GET -> Root / "rest" / "filterset" / filterset / "tags" => getFacetOptions(model, filterset, "tags")
    case GET -> Root / "rest" / "filterset" / filterset / "params" => getFacetOptions(model, filterset, "params")
    case GET -> Root / "rest" / "filterset" / filterset / "returns" => getFacetOptions(model, filterset, "returns")
    case GET -> Root / "rest" / "filterset" / filterset / "libs" => getFacetOptions(model, filterset, "libs")
    case GET -> Root / "rest" / "filterset" / filterset / "search"
      :? QueryQueryParamMatcher(query)
      :? LibsQueryParamMatcher(libs)
      :? ParamQueryParamMatcher(params)
      :? ReturnQueryParamMatcher(returns)
      :? TagQueryParamMatcher(tags)
      :? StartQueryParamMatcher(start)
      :? RowsQueryParamMatcher(rows)
      :? FacetQueryParamMatcher(facet) =>
    {
      Model.query(model, filterset, query.getOrElse(""), libs.getOrElse(List()), params.getOrElse(List()), returns.getOrElse(List()), tags.getOrElse(List()), start.getOrElse(0), rows.getOrElse(40)).flatMap {
        case Some(resp) => {
          val respToReturn = 
            if (facet.getOrElse(true)) resp
            else QueryResult(resp.total, List(), List(), List(), List(), resp.items)
          Ok(respToReturn.asJson)
        }
        case _ => NotFound()
      }
    }
    case GET -> Root / "rest" / "filterset" / filterset / lib / name => Model.get(model, lib, name).flatMap {
      case Some(e) => Ok(e)
      case _ => NotFound()
    }
  }.orNotFound

  def getFacetOptions[T : Indexer](model: Model[T], filterset: String, facetName: String) = {
    {
      for {
        libs <- EitherT.fromEither[IO](getLibs(model, filterset))
        tags <- EitherT(model.indexer.listFacetOptions(libs, facetName).map(x => Right(x)))
      } yield tags
    }.value.flatMap {
      case Right(tags) => Ok(tags.asJson)
      case Left(err) => err
    }
  }

  def getLibs[T: Indexer](model: Model[T], filterset: String): Either[IO[Response[IO]], List[String]] =
    model.filtersets
      .find(x => x.code == filterset)
      .map(x => x.libs)
      .toRight(NotFound("Filterset not found"))

  def runServer[T : Indexer](model: Model[T]): IO[ExitCode] = {
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(makeRoutes(model))
      .withErrorHandler(err => IO {
        log.error("Error handling", err)
        Response(InternalServerError)
      })
      .build
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }
}
