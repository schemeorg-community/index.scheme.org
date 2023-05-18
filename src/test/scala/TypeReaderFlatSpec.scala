import org.scalatest.funspec.AnyFunSpec
import scmindex.core.*
import scmindex.*
import scmindex.given
import cats.effect.unsafe.implicits.global

class TypeReaderFlatSpec extends AnyFunSpec {

  describe("Loading types") {
    it("should work") {
      val config = Config(
        80,
        "types/index.scm",
        false,
        "",
        "",
        "",
        "")
      SCMIndexEntry.loadSignatures(config).unsafeRunSync() match {
        case Right(signatures) => {
          assert(signatures.size > 0)
        }
        case Left(err) => {
          err.printStackTrace()
          fail(err)
        }
      }
    }
  }

}
