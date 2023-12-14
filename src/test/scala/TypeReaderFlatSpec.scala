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
        "db",
        "types/index.scm",
        false,
        "",
        "",
        "",
        "")
      SCMIndexEntry.loadSignatures(config).unsafeRunSync() match {
        case signatures => {
          assert(signatures.size > 0)
        }
      }
    }
  }

}
