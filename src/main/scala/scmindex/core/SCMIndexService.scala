package scmindex.core

import cats.effect.IO
import cats.implicits._
import cats.data.OptionT
import cats.data.EitherT

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
  (using Indexer[T, ID], Storage[S, ID]): IO[Option[QueryResult]] = {
      val optt = for {
        filtersets <- OptionT.liftF(service.storage.getFiltersets())
        filterset <- OptionT.fromOption[IO](filtersets.find(f => f.code == filtersetCode))
        libsToSolr = 
          if libs.isEmpty 
          then filterset.libs
          else libs.intersect(filterset.libs)
        resp <- OptionT.liftF(service.indexer.query(queryString, libsToSolr, params, returns, tags, rows, start))
        entriesIO = resp.items
          .map { id => service.storage.get(id) }
          .sequence
          .map {lst => lst.flatMap(_.toList)}
        entries <- OptionT.liftF(entriesIO)
      } yield QueryResult(resp.total, resp.libs, resp.params, resp.returns, resp.tags, entries)
      optt.value
  }

  def get[T, S, ID](
    service: SCMIndexService[T, S],
    lib: String,
    name: String)
  (using Indexer[T, ID], Storage[S, ID]): IO[Option[SCMIndexEntry]] = {
    val e = for {
      id <- OptionT(service.indexer.get(lib, name))
      entry <- OptionT(service.storage.get(id))
    } yield entry
    e.value
  }

  def getFiltersets[T, S, ID](service: SCMIndexService[T, S])(using Storage[S, ID]): IO[List[Filterset]] = {
    service.storage.getFiltersets()
  }

  def getFacetOptions[T, S, ID](service: SCMIndexService[T, S], filterset: String, facetName: String)(using Indexer[T, ID], Storage[S, ID]): IO[Option[List[String]]]
  = {
    val optT = for {
      filtersets <- OptionT.liftF(service.storage.getFiltersets())
      maybeLibs = filtersets
        .find(x => x.code == filterset)
        .map(x => x.libs)
      libs <- OptionT.fromOption(maybeLibs)
      facets <- OptionT.liftF(service.indexer.listFacetOptions(libs, facetName))
    } yield facets
    optT.value
  }

  def runImport[T, S, ID, I](service: SCMIndexService[T, S], importer: I)(using Indexer[T, ID], Storage[S, ID], Importer[I]): IO[Either[Exception, Unit]] = {
    val eitherT = for {
      filtersets <- EitherT(Filterset.loadFiltersets(importer))
      indexEntries <- EitherT(SCMIndexEntry.loadSignatures(importer))
      _ <- EitherT.liftF(service.storage.deleteAll())
      _ <- EitherT.liftF(service.storage.saveFiltersets(filtersets))
      saved <- EitherT.liftF(service.storage.save(indexEntries))
      _ <- EitherT.liftF(service.indexer.index(saved))
    } yield ()
    eitherT.value
  }

}
