package supercalc

import org.scalatest.FunSuite

class ParserTest extends FunSuite {

//   test("Parser.parse") {
//     // val tokens = Tokenizer.tokenize("1 add (23 multiple 456)")
//     val parsed = Parser.parse(List(
//       Number(1),
//       CommandAdd,
//       ParenOpen,
//       Number(23),
//       CommandMultiple,
//       Number(456),
//       ParenClose
//     ))
//     val expect = Node(
//       op = CommandAdd,
//       left = Leaf(Number(1)),
//       right = Node(
//         op = CommandMultiple,
//         left = Leaf(Number(23)),
//         right = Leaf(Number(456))
//       )
//     )

//     assert(parsed == expect)
//   }

  test("Nest") {
    val tokens = List(
      Number(1),
      Number(2),
      ParenOpen,
      Number(3),
      Number(4),
      Number(5),
      ParenClose
    )
    val nested = Parser.nest(tokens)

    println(nested)

    val expect: List[Any] = List(
      Number(1),
      Number(2),
      List(
        Number(3),
        Number(4),
        Number(5)
      )
    )

    assert(nested == expect)
  }

  test("Nest2") {
    val tokens = List(
      Number(1),
      Number(2),
      ParenOpen,
      ParenOpen,
      Number(3),
      Number(4),
      ParenClose,
      Number(5),
      ParenClose
    )
    val nested = Parser.nest(tokens)

    println(nested)

    val expect: List[Any] = List(
      Number(1),
      Number(2),
      List(
        List(
          Number(3),
          Number(4)
        ),
        Number(5)
      )
    )

    assert(nested == expect)
  }

  test("NestFail") {
    val tokens = List(
      Number(1),
      Number(2),
      ParenClose,
      ParenOpen,
      ParenOpen,
      Number(3),
      Number(4),
      ParenClose,
      ParenClose,
      Number(5),
    )
    val nested = Parser.nest(tokens)

    println(nested)

    val expect: List[Any] = List(
      Number(1),
      Number(2),
      List(
        List(
          Number(3),
          Number(4)
        ),
        Number(5)
      )
    )

    assert(nested == expect)
  }

}