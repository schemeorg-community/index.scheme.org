import cats.effect.IO
import org.scalatest.funspec.AnyFunSpec
import scmindex.*
import cats.effect.unsafe.implicits.global

class FiltersetFlatSpec extends AnyFunSpec {

  describe("Given valid sexpr") {
    it("should parse it") {
      val index = SexprParser.read("""
        (((name . "testname") ;; test comment
         (code . "testcode")
         (file . "testfile")))
        """) match {
        case Right(s) => s
        case Left(err) => fail(err)
      }
      val loader = (file: String) => {
        if (file == "testfile") {
          IO.pure(SexprParser.read("(((foo bar) . #t) ((foo baz) . #f))"))
        } else  {
          IO.pure(Left(Exception("")))
        }
      }
      Filterset.loadFiltersets(index, loader).unsafeRunSync() match {
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
