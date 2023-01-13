package scmindex

case class Facet(name: String, values: List[(String, Int)])
case class Response(total: Int, facets: List[Facet], items: List[Int])

trait Indexer {

  def index(entries: Vector[SCMIndexEntry]): Unit
  def query(query: String, lib: List[String], param: List[String], returns: List[String], pageSize: Int, offset: Int): Response

}
