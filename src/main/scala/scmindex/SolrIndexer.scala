package scmindex

import java.nio.file.Path
import org.apache.solr.client.solrj.{SolrClient, SolrQuery}
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.Http2SolrClient
import org.apache.solr.common.params.SolrParams
import org.apache.solr.common.{SolrDocument, SolrInputDocument}
import scmindex.SolrIndexer.indexEntryToSolr

case class SolrIndexer(solrClient: SolrClient) extends Indexer {

  override def index(entries: Vector[SCMIndexEntry]): Unit = {
    for (index <- Range(0, entries.size)) {
      solrClient.add(indexEntryToSolr(index, entries(index)), 5000)
    }
  }

  override def query(query: String, lib: List[String], param: List[String], returns: List[String], pageSize: Int, offset: Int): Response = {
    val params = new SolrQuery()
    params.setQuery(query)
    val response = solrClient.query(params)
    ???
  }

}

object SolrIndexer {

  def createEmbeddedSolrIndexer(solrHome: String, core: String): SolrIndexer =
    SolrIndexer(new EmbeddedSolrServer(Path.of(solrHome), core))

  def createRemoteSolrIndexer(solrUrl: String): SolrIndexer =
    SolrIndexer(new Http2SolrClient.Builder(solrUrl).build())

  private def indexEntryToSolr(index: Int, e: SCMIndexEntry): SolrInputDocument = {
    val d = new SolrInputDocument()
    d.setField("index", index)
    d.setField("name", e.name)
    d.setField("lib", e.lib)
    d
  }

}