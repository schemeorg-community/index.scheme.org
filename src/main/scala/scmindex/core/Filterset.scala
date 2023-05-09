package scmindex.core

import cats.data.EitherT
import cats.effect.IO
import cats.implicits.*

case class Filterset(code: String, name: String, libs: List[String])

object Filterset {

  def loadFiltersets[T: Importer](importer: T): IO[Either[Exception, List[Filterset]]] = {
    val eitherT = for {
      index <- EitherT(importer.loadFiltersetIndex())
      indexAsList <- EitherT.fromEither(Sexpr.sexprToProperList(index))
      filtersets <- EitherT({
        indexAsList
          .map({parseFilterset(_, importer.loadFilterset)})
          .sequence
          .map { f =>
            f.partitionMap(identity) match {
              case (Nil, rights) => Right(rights)
              case (first :: _, _) => Left(Exception("Failed to parse filtersets", first))
            }
          }
      })
    } yield filtersets
    eitherT.value
  }

  def parseFilterset(sexpr: Sexpr, reader: (String) => IO[Either[Exception, Sexpr]]): IO[Either[Exception, Filterset]] = {
    def getStringField(map: Map[String, Sexpr], field: String): Either[Exception, String] = {
      map.get(field) match {
        case Some(SexprString(value)) => Right(value)
        case Some(v) => Left(Exception(s"Expected string value for field ${field}, got: ${v.toString}"))
        case None => Left(Exception(s"Missing ${field} field"))
      }
    }
    val res = for {
      map <- EitherT.fromEither[IO](Sexpr.alistToMap(sexpr))
      code <- EitherT.fromEither(getStringField(map, "code"))
      name <- EitherT.fromEither(getStringField(map, "name"))
      file <- EitherT.fromEither(getStringField(map, "file"))
      libsSexpr <- EitherT(reader(file))
      libs <- EitherT.fromEither(parseFiltersetLibs(libsSexpr))
    } yield Filterset(code, name, libs)
    res.value
  }

  def parseFiltersetLibs(sexpr: Sexpr): Either[Exception, List[String]] = {
    Sexpr.sexprToProperList(sexpr)
      .flatMap { terms =>
        val entries = terms.map {
          case SexprPair(car, SexprBool(value)) => Right((car.toString, value))
          case v => Left(Exception(s"Expected a pair, got: ${v}"))
        }
        entries.partitionMap(identity) match {
          case (Nil, e) => Right(e.filter(_._2).map(_._1))
          case (first :: _, _) => Left(Exception("Failed to parse filterset libs", first))
        }
      }
  }
}
