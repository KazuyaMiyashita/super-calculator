package supercalc.tokenize

import org.scalatest.FunSuite

class TokenizerTest extends FunSuite {

  test("Tokenizer.tokenize") {
    val input = "1 add (23 multiple 456)"
    val tokens = Tokenizer.tokenize(input)

    val expect = List(
      Number(1),
      CommandAdd,
      ParenOpen,
      Number(23),
      CommandMultiple,
      Number(456),
      ParenClose
    )

    assert(tokens == expect)
  }

}