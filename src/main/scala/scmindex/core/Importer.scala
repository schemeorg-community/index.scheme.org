package scmindex.core

import cats.effect.IO

trait Importer[A] {

  extension (a :A) {
    def loadIndex(): IO[Sexpr]
    def loadLibrary(file: String): IO[Sexpr]
    def loadFiltersetIndex(): IO[Sexpr]
    def loadFilterset(src: String): IO[Sexpr]
  }

}
