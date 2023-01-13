package scmindex

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class DefaultSignatureLoader(index: String) extends SignatureLoader {

  override def loadIndex(): Either[String, Sexpr] = {
    loadLibrary(index)
  }

  override def loadLibrary(file: String): Either[String, Sexpr] = {
    val path = Path.of(file)
    val content = Files.readString(path, StandardCharsets.UTF_8)
    read(content)
  }

}
