package scmindex

import cats.effect.IO

trait SignatureLoader[A] {

  extension (a :A) {
    def loadIndex(): IO[Either[Exception, Sexpr]]
    def loadLibrary(file: String): IO[Either[Exception, Sexpr]]
  }

}
