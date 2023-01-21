package scmindex

import cats.effect.IO
import cats.data.OptionT

case class Model[T : Indexer](indexer: T, config: Config, filtersets: List[Filterset], entries: Vector[SCMIndexEntry])

case class QueryResult(total: Int, libs: List[FacetValue], params: List[FacetValue], returns: List[FacetValue], tags: List[FacetValue], items: List[SCMIndexEntry])

object Model {

  def query[T: Indexer](model: Model[T],
                        filtersetCode: String,
                        queryString: String,
                        libs: List[String],
                        params: List[String],
                        returns: List[String],
                        tags: List[String],
                        page: Int): IO[Option[QueryResult]] = {
    val optt = for {
      filterset <- OptionT.fromOption[IO](model.filtersets.find(f => f.code == filtersetCode))
      libsToSolr = if libs.isEmpty then filterset.libs else libs.intersect(filterset.libs)
      resp <- OptionT.liftF(model.indexer.query(queryString, libsToSolr, params, returns, tags, model.config.pageSize, model.config.pageSize * page))
    } yield makeQueryResult(model.entries, resp)
    optt.value
  }

  def makeQueryResult(entries: Vector[SCMIndexEntry], resp: IndexerResponse): QueryResult = {
    val indexEntries = resp.items.map(entries(_))
    //QueryResult(indexEntries, resp.facets, resp.total)
    QueryResult(resp.total, resp.libs, resp.params, resp.returns, resp.tags, indexEntries)
  }

}
