import org.scalatest.funspec.AnyFunSpec
import scmindex.*

class TypeReaderFlatSpec extends AnyFunSpec {

  describe("Loading types") {
    it("should work") {
      val loader = new DefaultSignatureLoader("types/index.scm")
      loadSignatures(loader) match {
        case Right(signatures) => {
          assert(signatures.size > 0)
        }
        case Left(err) => fail(err)
      }
    }
  }

}
