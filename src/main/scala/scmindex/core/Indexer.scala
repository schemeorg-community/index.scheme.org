package scmindex.core

import cats.effect.IO

case class FacetValue(value: String, count: Int)
case class IndexerResponse[R](total: Int, libs: List[FacetValue], params: List[FacetValue], returns: List[FacetValue], tags: List[FacetValue], items: List[R])

trait Indexer[A, ID] {
  extension (a: A) {
    def index(entries: List[(ID, SCMIndexEntry)]): IO[Unit]
    def query(query: String, lib: List[String], param: List[String], returns: List[String], tags: List[String], pageSize: Int, offset: Int): IO[IndexerResponse[ID]]
    def get(lib: String, name: String): IO[Option[ID]]
    def listFacetOptions(lib: List[String], facetName: String): IO[List[String]]
  }
}
