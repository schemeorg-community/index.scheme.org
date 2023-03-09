package scmindex

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import cats.effect.IO
import org.slf4j.LoggerFactory

case class Config(
  port: Int,
  specIndex: String,
  solrEmbed: Boolean,
  solrHome: String,
  solrUrl: String,
  solrCore: String,
  filtersetIndex: String)

object Config {

  def log = LoggerFactory.getLogger("config")

  def readFromSexpr(sexpr: Sexpr): Either[Exception, Config] = {
    def getString(map: Map[String, Sexpr], key: String, default: String): Either[Exception, String] = {
      map.get(key) match {
        case Some(SexprString(content)) => Right(content)
        case None => Right(default)
        case v => Left(Exception(s"Expected ${key} to be a string, was ${v}"))
      }
    }
    def getInt(map: Map[String, Sexpr], key: String, default: Int): Either[Exception, Int] = {
      map.get(key) match {
        case Some(SexprNumber(content)) => Right(content)
        case None => Right(default)
        case v => Left(Exception(s"Expected ${key} to be an integer, was ${v}"))
      }
    }
    def getBool(map: Map[String, Sexpr], key: String, default: Boolean): Either[Exception, Boolean] = {
      map.get(key) match {
        case Some(SexprBool(content)) => Right(content)
        case None => Right(default)
        case v => Left(Exception(s"Expected ${key} to be a boolean, was ${v}"))
      }
    }
    for {
      map <- Sexpr.alistToMap(sexpr)
      port <- getInt(map, "port", 8080)
      specIndex <- getString(map, "spec-index", "types/index.scm")
      solrEmbed <- getBool(map, "solr-embed", true)
      solrHome <- getString(map, "solr-home", "./solrhome")
      solrUrl <- getString(map, "solr-url", "http://localhost:8983/solr")
      solrCore <- getString(map, "solr-core", "scmindex")
      filtersetIndex <- getString(map, "filterset-idnex", "filters/index.scm")
    } yield Config(port, specIndex, solrEmbed, solrHome, solrUrl, solrCore, filtersetIndex)
  }

  given SignatureLoader[Config] with {
    extension (config: Config) {
      def loadIndex() = config.loadLibrary(config.specIndex)

      def loadLibrary(file: String) = IO {
        val path = Path.of(file)
        val content = Files.readString(path, StandardCharsets.UTF_8)
        log.info(s"Loading library ${file}")
        SexprParser.read(content)
      }
    }
  }

}
