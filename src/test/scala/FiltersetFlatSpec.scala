import cats.effect.IO
import org.scalatest.funspec.AnyFunSpec
import scmindex.core.*
import scmindex.*
import scmindex.given
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
          SexprParser.read("(((foo bar) . #t) ((foo baz) . #f))")
        } else  {
          Left(Exception(""))
        }
      }

      case class TestImporter() extends Importer[Any] {
        extension (a: Any) {
          override def loadIndex(): IO[Either[Exception, Sexpr]] = IO.pure(Left(Exception("")))
          override def loadLibrary(file: String): IO[Either[Exception, Sexpr]] = IO.pure(Left(Exception("")))
          override def loadFiltersetIndex(): IO[Either[Exception, Sexpr]] = IO.pure(Right(index))
          override def loadFilterset(src: String): IO[Either[Exception, Sexpr]] = IO.pure(loader(src))
        } 
      }

      Filterset.loadFiltersets(())(using TestImporter()).unsafeRunSync() match {
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
