package scmindex;

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import scmindex.SolrIndexer.given

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val configFile = args match {
      case Nil => "./config/configuration.scm"
      case x :: _ => x
    }

    {for {
      configSexpr <- EitherT(SexprParser.readFromFile(configFile))
      cfg <- EitherT.fromEither(Config.readFromSexpr(configSexpr))
      entriesList <- EitherT(SCMIndexEntry.loadSignatures(cfg))
      entries = entriesList.toVector
      filtersetsSexpr <- EitherT(SexprParser.readFromFile(cfg.filtersetIndex))
      filtersetsReader = (str: String) => SexprParser.readFromFile(str)
      filtersets <- EitherT(Filterset.loadFiltersets(filtersetsSexpr, filtersetsReader))
      solrIndexer = if (cfg.solrEmbed) SolrIndexer.createEmbeddedSolrIndexer(cfg.solrHome, cfg.solrCore) else SolrIndexer.createRemoteSolrIndexer(cfg.solrUrl)
      _ <- EitherT(solrIndexer.index(entries).map {_ => Right(())})
      model = Model(solrIndexer, cfg, filtersets, entries)
    } yield model}.value.flatMap {
      case Right(model) => WebController.runServer(model)
      case Left(err) => IO(err.printStackTrace()).as(ExitCode(1))
    }
  }
}
