package scmindex.core

import cats.effect.IO

trait Importer[A] {

  extension (a :A) {
    def loadIndex(): IO[Either[Exception, Sexpr]]
    def loadLibrary(file: String): IO[Either[Exception, Sexpr]]
    def loadFiltersetIndex(): IO[Either[Exception, Sexpr]]
    def loadFilterset(src: String): IO[Either[Exception, Sexpr]]
  }

}
