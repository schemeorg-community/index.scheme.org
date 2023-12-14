package scmindex.core

import cats.effect.IO
import cats.implicits._

case class SCMIndexService[T, S](indexer: T, storage: S)

case class QueryResult(total: Int, libs: List[FacetValue], params: List[FacetValue], returns: List[FacetValue], tags: List[FacetValue], items: List[SCMIndexEntry])

object SCMIndexService {

  def query[T, S, ID](
    service: SCMIndexService[T, S],
    filtersetCode: String,
    queryString: String,
    libs: List[String],
    params: List[String],
    returns: List[String],
    tags: List[String],
    start: Int,
    rows: Int)
  (using Indexer[T, ID], Storage[S, ID]): IO[QueryResult] = {
      for {
        filtersets <- service.storage.getFiltersets()
        filterset <- filtersets.find(f => f.code == filtersetCode).liftTo[IO](Exception("Filterset not found"))
        libsToSolr = 
          if libs.isEmpty 
          then filterset.libs
          else libs.intersect(filterset.libs)
        resp <- service.indexer.query(queryString, libsToSolr, params, returns, tags, rows, start)
        entries <- service.storage.get(resp.items).map {lst => lst.flatMap(_.toList)}
      } yield QueryResult(resp.total, resp.libs, resp.params, resp.returns, resp.tags, entries)
  }

  def get[T, S, ID](
    service: SCMIndexService[T, S],
    lib: String,
    name: String)
  (using Indexer[T, ID], Storage[S, ID]): IO[Option[SCMIndexEntry]] = {
    for {
      id <- service.indexer.get(lib, name).flatMap{_.liftTo[IO](Exception("Signature not found"))}
      entry <- service.storage.get(List(id)).map(lst => lst.get(0).get)
    } yield entry
  }

  def getFiltersets[T, S, ID](service: SCMIndexService[T, S])(using Storage[S, ID]): IO[List[Filterset]] = {
    service.storage.getFiltersets()
  }

  def getFacetOptions[T, S, ID](service: SCMIndexService[T, S], filterset: String, facetName: String)(using Indexer[T, ID], Storage[S, ID]): IO[List[String]]
  = {
    for {
      filtersets <- service.storage.getFiltersets()
      libs <- filtersets
        .find(x => x.code == filterset)
        .map(x => x.libs)
        .liftTo[IO](Exception("Filterset not found"))
      facets <- service.indexer.listFacetOptions(libs, facetName)
    } yield facets
  }

  def runImport[T, S, ID, I](service: SCMIndexService[T, S], importer: I)(using Indexer[T, ID], Storage[S, ID], Importer[I]): IO[Unit] = {
    for {
      filtersets <- Filterset.loadFiltersets(importer)
      indexEntries <- SCMIndexEntry.loadSignatures(importer)
      _ <- service.storage.deleteAll()
      _ <- service.storage.saveFiltersets(filtersets)
      saved <- service.storage.save(indexEntries)
      _ <- service.indexer.index(saved)
    } yield ()
  }

}
