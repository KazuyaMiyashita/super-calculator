package supercalc

import org.scalatest.FunSuite

class NestTest extends FunSuite {

  test("Nest") {
    val nested: Nest[Char] = NestList(
      NestLeaf('a'),
      NestLeaf('a'),
      NestList(
        NestLeaf('a'),
        NestLeaf('a'),
        NestLeaf('a')
      )
    )

    println(nested)
    assert(true)
  }

}