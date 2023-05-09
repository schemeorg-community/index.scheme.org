package scmindex.core

import cats.effect.IO

trait Storage[T, ID] {
  extension(t: T) {

    def init(): IO[Unit]
    def save(lst: List[SCMIndexEntry]): IO[List[(ID, SCMIndexEntry)]]
    def get(id: ID): IO[Option[SCMIndexEntry]]

    def saveFiltersets(f: List[Filterset]): IO[Unit]
    def getFiltersets(): IO[List[Filterset]]
  }
}
