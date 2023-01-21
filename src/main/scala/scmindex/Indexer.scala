package scmindex

import cats.effect.IO

case class FacetValue(value: String, count: Int)
case class IndexerResponse(total: Int, libs: List[FacetValue], params: List[FacetValue], returns: List[FacetValue], tags: List[FacetValue], items: List[Int])

//parameterize return type from Int
trait Indexer[A] {
  extension (a: A) {
    def index(entries: Vector[SCMIndexEntry]): IO[Unit]
    def query(query: String, lib: List[String], param: List[String], returns: List[String], tags: List[String], pageSize: Int, offset: Int): IO[IndexerResponse]
    def listFacetOptions(lib: List[String], facetName: String): IO[List[String]]
  }
}