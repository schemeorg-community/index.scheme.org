package scmindex

trait SignatureLoader {

  def loadIndex(): Either[String, Sexpr]
  def loadLibrary(file: String): Either[String, Sexpr]

}
