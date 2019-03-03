package supercalc

import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("Tokenizer.tokenize") {
    // val tokens = Tokenizer.tokenize("1 add (23 multiple 456)")
    val parsed = Parser.parse(List(
      Number(1),
      CommandAdd,
      ParenOpen,
      Number(23),
      CommandMultiple,
      Number(456),
      ParenClose
    ))
    val expect = Node(
      op = CommandAdd,
      left = Leaf(Number(1)),
      right = Node(
        op = CommandMultiple,
        left = Leaf(Number(23)),
        right = Leaf(Number(456))
      )
    )

    assert(parsed == expect)
  }

}