package scmindex;

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import cats.data.EitherT
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

    val modelEitherT = for {
      configSexpr <- EitherT(SexprParser.readFromFile(configFile))
      cfg <- EitherT.fromEither(Config.readFromSexpr(configSexpr))
      solrIndexer =
        if (cfg.solrEmbed) 
          SolrIndexer.createEmbeddedSolrIndexer(cfg.solrHome, cfg.solrCore)
        else 
          SolrIndexer.createRemoteSolrIndexer(cfg.solrUrl)
      storage = SqliteStorage.create("db") // TODO parameterize
      service = SCMIndexService(solrIndexer, storage)
      _ <- EitherT(SCMIndexService.runImport(service, cfg))
      exitCode <- EitherT.liftF(WebController.runServer(service, cfg.port))
    } yield exitCode

    modelEitherT.value.flatMap {
      case Right(exitCode) => IO(exitCode)
      case Left(err) => IO(err.printStackTrace()).as(ExitCode(1))
    }
  }
}
