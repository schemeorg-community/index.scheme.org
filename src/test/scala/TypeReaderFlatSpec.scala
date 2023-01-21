import org.scalatest.funspec.AnyFunSpec
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
        0,
        "",
        "")
      SCMIndexEntry.loadSignatures(config).unsafeRunSync() match {
        case Right(signatures) => {
          assert(signatures.size > 0)
        }
        case Left(err) => fail(err)
      }
    }
  }

}
