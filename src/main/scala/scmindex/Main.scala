package scmindex;

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import cats.effect.{ExitCode, IO, IOApp}
import scmindex.persistance.*
import scmindex.persistance.SolrIndexer.given
import scmindex.core.*
import scmindex.web.WebController
import scmindex.Config.given

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val configFile = args match {
      case Nil => "./config/configuration.scm"
      case x :: _ => x
    }

    for {
      configSexpr <- SexprParser.readFromFile(configFile)
      cfg <- IO.fromEither(Config.readFromSexpr(configSexpr))
      solrIndexer =
        if (cfg.solrEmbed) 
          SolrIndexer.createEmbeddedSolrIndexer(cfg.solrHome, cfg.solrCore)
        else 
          SolrIndexer.createRemoteSolrIndexer(cfg.solrUrl)
      storage <- SqliteStorage.create(cfg.dbPath)
      _ <- storage.init()
      service = SCMIndexService(solrIndexer, storage)
      _ <- SCMIndexService.runImport(service, cfg)
      exitCode <- WebController.runServer(service, cfg.port)
    } yield exitCode
  }
}
