package scmindex

case class Config(
  port: Int,
  specIndex: String,
  solrEmbed: Boolean,
  solrHome: String,
  solrUrl: String,
  solrCore: String,
  pageSize: Int,
  filtersetIndex: String,
  downloadsConfig: String)

object Config {

  def readFromSexpr(sexpr: Sexpr): Either[String, Config] = {
    def getString(map: Map[String, Sexpr], key: String, default: String): Either[String, String] = {
      map.get(key) match {
        case Some(SexprString(content)) => Right(content)
        case None => Right(default)
        case v => Left(s"Expected ${key} to be a string, was ${v}")
      }
    }
    def getInt(map: Map[String, Sexpr], key: String, default: Int): Either[String, Int] = {
      map.get(key) match {
        case Some(SexprNumber(content)) => Right(content)
        case None => Right(default)
        case v => Left(s"Expected ${key} to be an integer, was ${v}")
      }
    }
    def getBool(map: Map[String, Sexpr], key: String, default: Boolean): Either[String, Boolean] = {
      map.get(key) match {
        case Some(SexprBool(content)) => Right(content)
        case None => Right(default)
        case v => Left(s"Expected ${key} to be a boolean, was ${v}")
      }
    }
    for {
      map <- alistToMap(sexpr)
      port <- getInt(map, "port", 8080)
      specIndex <- getString(map, "spec-index", "types/index.scm")
      solrEmbed <- getBool(map, "solr-embed", true)
      solrHome <- getString(map, "solr-home", "./solrhome")
      solrUrl <- getString(map, "solr-url", "http://localhost:8983/solr")
      solrCore <- getString(map, "solr-core", "scmindex")
      pageSize <- getInt(map, "page-size", 40)
      filtersetIndex <- getString(map, "filterset-idnex", "filters/index.scm")
      downloadsConfig <- getString(map, "downloads-config", "./config/downloads.scm")
    } yield Config(port, specIndex, solrEmbed, solrHome, solrUrl, solrCore, pageSize, filtersetIndex, downloadsConfig)
  }

}