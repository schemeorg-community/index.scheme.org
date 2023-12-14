package scmindex.core

import cats.effect.IO
import cats.implicits.*

case class Filterset(code: String, name: String, libs: List[String])

object Filterset {

  def loadFiltersets[T: Importer](importer: T): IO[List[Filterset]] = {
    for {
      index <- importer.loadFiltersetIndex()
      indexAsList <- Sexpr.sexprToProperList(index).liftTo[IO]
      filtersets <- {
        indexAsList
          .map({parseFilterset(_, importer.loadFilterset)})
          .sequence
      }
    } yield filtersets
  }

  def parseFilterset(sexpr: Sexpr, reader: (String) => IO[Sexpr]): IO[Filterset] = {
    def getStringField(map: Map[String, Sexpr], field: String): IO[String] = {
      map.get(field) match {
        case Some(SexprString(value)) => IO.pure(value)
        case Some(v) => IO.raiseError(Exception(s"Expected string value for field ${field}, got: ${v.toString}"))
        case None => IO.raiseError(Exception(s"Missing ${field} field"))
      }
    }
    for {
      map <- IO.pure(Sexpr.alistToMap(sexpr)).rethrow
      code <- getStringField(map, "code")
      name <- getStringField(map, "name")
      file <- getStringField(map, "file")
      libsSexpr <- reader(file)
      libs <- IO.pure(parseFiltersetLibs(libsSexpr)).rethrow
    } yield Filterset(code, name, libs)
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
