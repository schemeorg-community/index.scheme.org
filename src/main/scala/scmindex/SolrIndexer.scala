package scmindex

import cats.effect.IO

import java.nio.file.Path
import org.apache.solr.client.solrj.{SolrClient, SolrQuery}
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.Http2SolrClient
import org.apache.solr.common.params.SolrParams
import org.apache.solr.common.{SolrDocument, SolrInputDocument}
import scala.jdk.CollectionConverters._
import org.apache.solr.client.solrj.util.ClientUtils
import org.apache.solr.client.solrj.response.FacetField

object SolrIndexer {

  def createEmbeddedSolrIndexer(solrHome: String, core: String) =
    new EmbeddedSolrServer(Path.of(solrHome), core)

  def createRemoteSolrIndexer(solrUrl: String) =
    new Http2SolrClient.Builder(solrUrl).build()

  private def indexEntryToSolr(index: Int, e: SCMIndexEntry): SolrInputDocument = {
    val d = new SolrInputDocument()
    d.setField("index", index)
    d.setField("name", e.name)
    d.setField("lib", e.lib)
    d.setField("tags", e.tags.asJava)
    d.setField("param_names", SCMIndexEntry.paramNames(e).asJava)
    d.setField("param_types", SCMIndexEntry.paramTypes(e).asJava)
    d.setField("return_types", SCMIndexEntry.returnTypes(e).asJava)
    d.setField("description", e.description)
    d
  }

  given Indexer[SolrClient] with
    extension (c: SolrClient) {
      def index(entries: Vector[SCMIndexEntry]) = IO {
        c.deleteByQuery("*:*")
        for (index <- Range(0, entries.size)) {
          c.add(indexEntryToSolr(index, entries(index)), 1000)
        }
      }
      def parseFacet(f: FacetField): List[FacetValue] = {
        f.getValues.asScala.toList.map(c => FacetValue(c.getName, c.getCount.toInt))
      }

      def get(lib: String, name: String): IO[Option[Int]] = IO {
        val q = SolrQuery();
        q.setRequestHandler("/search")
        q.setQuery("")
        q.setRows(1);
        q.setFilterQueries(s"name_precise: ${ClientUtils.escapeQueryChars(name)}", s"lib: ${ClientUtils.escapeQueryChars(lib)}")
        val resp = c.query(q);
        if (resp.getResults.getNumFound == 0)
          None
        else
          Some(resp.getResults.get(0).getFieldValue("index").asInstanceOf[Int])
      }

      def query(query: String, lib: List[String], param: List[String], returns: List[String], tags: List[String], pageSize: Int, offset: Int): IO[IndexerResponse] = IO {
        val q = SolrQuery();
        q.setRequestHandler("/search")
        q.setQuery(query)
        q.setRows(pageSize)
        q.setStart(offset)
        q.setFields("index")
        val fqs = List.concat(
          makeMultivalueClause("lib", lib, "OR"),
          makeMultivalueClause("param_types", param, "AND"),
          makeMultivalueClause("return_types", returns, "AND"),
          makeMultivalueClause("tags", tags, "AND")
        )
        q.setFilterQueries(fqs:_*)
        val resp = c.query(q)
        val facets: Map[String, List[FacetValue]] = resp.getFacetFields.asScala
          .map((e: FacetField) => (e.getName, parseFacet(e)))
          .to(Map)
        val libsF = facets.getOrElse("lib", List())
        val paramsF = facets.getOrElse("param_types", List())
        val returnsF = facets.getOrElse("return_types", List())
        val tagsF = facets.getOrElse("tags", List())
        val indeces = resp.getResults.asScala.map(d => d.getFieldValue("index").asInstanceOf[Int]).toList
        IndexerResponse(resp.getResults.getNumFound.toInt, libsF, paramsF, returnsF, tagsF, indeces)
      }

      def listFacetOptions(lib: List[String], facetName: String): IO[List[String]] = IO {
        val q = SolrQuery();
        q.setRows(0)
        q.setFilterQueries(makeMultivalueClause("lib", lib, "OR"):_*)
        q.setRequestHandler("/search")
        val resp = c.query(q)
        resp.getFacetFields.asScala
          .filter(f => f.getName == facetName)
          .flatMap(field => field.getValues.asScala)
          .filter(c => c.getCount > 0)
          .map(c => c.getName)
          .toList
      }

      def makeMultivalueClause(field: String, params: List[String], operator: String): List[String] = {
        if (params.isEmpty)
          List()
        else {
          val str = params
            .map(e => s"\"${ClientUtils.escapeQueryChars(e)}\"")
            .mkString(s" ${operator} ");
          List(s"${field}: (${str})")
        }
      }
    }
}
