package scmindex

case class Filterset(code: String, name: String, libs: List[String])

def loadFiltersets(index: Sexpr, reader: (String) => Option[Sexpr]): Either[String, List[Filterset]] = {
  index match {
    case pair: SexprPair =>
      val (items, tail) = pairToList(pair)
      val grouped = items.map(parseFilterset(_, reader)).partitionMap(identity)
      grouped match {
          case (Nil, rights) => Right(rights)
          case (lefts, _) => Left(lefts.mkString(", "))
      }
    case _ => Left("Bad filterset index format, expected a list")
  }
}

private def parseFilterset(sexpr: Sexpr, stringToSexpr: String => Option[Sexpr]): Either[String, Filterset] = {
  def getStringField(map: Map[String, Sexpr], field: String): Either[String, String] = {
    map.get(field) match {
      case Some(SexprString(value)) => Right(value)
      case Some(v) => Left(s"Expected string value for field ${field}, got: ${v.toString}")
      case None => Left(s"Missing ${field} field")
    }
  }
  for {
    map <- alistToMap(sexpr)
    code <- getStringField(map, "code")
    name <- getStringField(map, "name")
    file <- getStringField(map, "file")
    libsSexpr <- stringToSexpr(file).toRight(left = s"Failed to load file ${file}")
    libs <- parseFiltersetLibs(libsSexpr)
  } yield Filterset(code, name, libs)
}

private def parseFiltersetLibs(sexpr: Sexpr): Either[String, List[String]] = {
  sexprToProperList(sexpr)
    .toRight("should be list")
    .flatMap { terms =>
      val entries = terms.map {
        case SexprPair(car, SexprBool(value)) => Right((car.toString, value))
        case v => Left(s"Expected a pair, got: ${v}")
      }
      entries.partitionMap(identity) match {
        case (Nil, e) => Right(e.filter(_._2).map(_._1))
        case (err, _) => Left(err.mkString(", "))
      }
    }
}