package scmindex;

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

@main
def main(args: String*): Unit = {
  val configFile = args match {
    case Nil => "./config/configuration.scm"
    case x :: _ => x
  }

  {for {
    configSexpr <- read(Files.readString(Path.of(configFile), StandardCharsets.UTF_8))
    cfg <- Config.readFromSexpr(configSexpr)
    loader = DefaultSignatureLoader(cfg.specIndex)
    entriesList <- loadSignatures(loader)
    entries = entriesList.toVector
    filtersetsSexpr <- read(Files.readString(Path.of(cfg.filtersetIndex), StandardCharsets.UTF_8))
    filtersetsReader = (str: String) => read(Files.readString(Path.of(str), StandardCharsets.UTF_8)).toOption
    filtersets <- loadFiltersets(filtersetsSexpr, filtersetsReader)
  } yield {
    val solrIndexer = if (cfg.solrEmbed) SolrIndexer.createEmbeddedSolrIndexer(cfg.solrHome, cfg.solrCore) else SolrIndexer.createRemoteSolrIndexer(cfg.solrUrl)
    solrIndexer.index(entries)
    (cfg, solrIndexer, filtersets, entries)
  }} match {
    case Right(_) => println("TODO")
    case Left(err) => println(err)
  }

}