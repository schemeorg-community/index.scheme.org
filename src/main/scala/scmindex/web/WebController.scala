package scmindex.web

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
import scmindex.core.*

object WebController {

  def log = LoggerFactory.getLogger("routes")

  object QueryQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("query")
  object StartQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Int]("start")
  object RowsQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Int]("rows")
  object LibsQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("lib")
  object ParamQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("param")
  object ReturnQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("return")
  object TagQueryParamMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("tag")
  object FacetQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Boolean]("facet")

  def makeRoutes[T, S, ID](model: SCMIndexService[T, S])(using Indexer[T, ID], Storage[S, ID]) = HttpRoutes.of[IO] {
    case GET -> Root / "rest" / "filterset" => for {
      filtersets <- SCMIndexService.getFiltersets(model)
      json = filtersets.map(f => Map("code" -> f.code, "name" -> f.name)).asJson
      resp <- Ok(json)
    } yield resp
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
      SCMIndexService.query(model, filterset, query.getOrElse(""), libs.getOrElse(List()), params.getOrElse(List()), returns.getOrElse(List()), tags.getOrElse(List()), start.getOrElse(0), rows.getOrElse(40)).flatMap {
        case Some(resp) => {
          val respToReturn = 
            if (facet.getOrElse(true)) resp
            else QueryResult(resp.total, List(), List(), List(), List(), resp.items)
          Ok(respToReturn.asJson)
        }
        case _ => NotFound()
      }
    }
    case GET -> Root / "rest" / "filterset" / filterset / lib / name => SCMIndexService.get(model, lib, name).flatMap {
      case Some(e) => Ok(e)
      case _ => NotFound()
    }
  }.orNotFound

  def getFacetOptions[T, S, ID](model: SCMIndexService[T, S], filterset: String, facetName: String)(using Indexer[T, ID], Storage[S, ID]) = {
    SCMIndexService.getFacetOptions(model, filterset, facetName).flatMap {
      case Some(options) => Ok(options.asJson)
      case _ => NotFound()
    }
  }

  def runServer[T, S, ID](service: SCMIndexService[T, S], port: Int)(using Indexer[T, ID], Storage[S, ID]): IO[ExitCode] = {
    for {
      parsedPort <- IO.fromOption(Port.fromInt(port))(Exception("Misconfigured port"))
      res <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(parsedPort)
        .withHttpApp(makeRoutes(service))
        .withErrorHandler(err => IO {
          log.error("Error handling", err)
          Response(InternalServerError)
        })
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
    } yield res
  }
}
