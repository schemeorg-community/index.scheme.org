import org.scalatest.funspec.AnyFunSpec
import scmindex.*

class LexerTest extends AnyFunSpec {
  describe("Given valid string") {
    it("should parse it") {
      val lexer = Lexer("""
                    "test \"foo\""
                    """);
      assert(lexer.next() == Some(StringLexeme("test \"foo\"")))
    }
  }
}
