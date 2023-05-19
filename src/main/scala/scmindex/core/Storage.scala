package scmindex.core

import cats.effect.IO

trait Storage[T, ID] {
  extension(t: T) {

    def deleteAll(): IO[Unit]

    def init(): IO[Unit]
    def save(lst: List[SCMIndexEntry]): IO[List[(ID, SCMIndexEntry)]]
    def get(ids: List[ID]): IO[List[Option[SCMIndexEntry]]]

    def saveFiltersets(f: List[Filterset]): IO[Unit]
    def getFiltersets(): IO[List[Filterset]]
  }
}
