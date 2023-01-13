import org.scalatest.funspec.AnyFunSpec
import scmindex.*

class FiltersetFlatSpec extends AnyFunSpec {

  describe("Given valid sexpr") {
    it("should parse it") {
      val index = read("""
        (((name . "testname") ;; test comment
         (code . "testcode")
         (file . "testfile")))
        """) match {
        case Right(s) => s
        case Left(err) => fail(err)
      }
      val loader = (file: String) => {
        if (file == "testfile") {
          read("(((foo bar) . #t) ((foo baz) . #f))").toOption
        } else  {
          None
        }
      }
      loadFiltersets(index, loader) match {
        case Right(filtersets) =>  {
          assert(1 == filtersets.size)
          val f = filtersets.head
          assert(f.code == "testcode")
          assert(f.name == "testname")
          assert(f.libs == List("(foo bar)"))
        }
        case Left(err) => fail(err)
      }
    }
  }

}
