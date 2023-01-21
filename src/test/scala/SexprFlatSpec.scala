import org.scalatest.funspec.AnyFunSpec
import scmindex.*

class SexprFlatSpec extends AnyFunSpec {
  describe("Pair to list unpacking") {
    val pair = SexprPair(SexprSymbol("a"), SexprPair(SexprSymbol("b"), SexprNull))
    it("should work") {
      val (lst, tail) = Sexpr.pairToList(pair)
      assert(tail == SexprNull)
      assert(lst == List(SexprSymbol("a"), SexprSymbol("b")))
    }
  }
  describe("alist to map unpacking") {
    val alist = "((test . \"foo\") (test2 bar baz) (desc . \"test\"))"
    it("should work") {
      SexprParser.read(alist)
        .flatMap { sexpr =>
          Sexpr.alistToMap(sexpr)
        } match {
        case Right(map) => {
          assert(map.get("test") == Some(SexprString("foo")))
          assert(map.get("test2") == Some(SexprPair(SexprSymbol("bar"), SexprPair(SexprSymbol("baz"), SexprNull))))
        }
      }
    }
  }
}
