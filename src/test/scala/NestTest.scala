package supercalc

import org.scalatest.FunSuite

class NestTest extends FunSuite {

  test("Nest.apply NestNil") {
    val nest = Nest()
    assert(nest == NestNil)
  }

  test("Nest.apply NestLeaf") {
    val nest = Nest('a')
    assert(nest == NestLeaf('a'))
  }

  test("Nest.apply NestList") {
    val nest = Nest('a', 'b')
    assert(nest == NestList(NestLeaf('a'), NestLeaf('b')))
  }

  test("Nest.apply nested NestList") {
    // val nest: Nest[Char] = Nest(Nest('a'), Nest(Nest(Nest('b'), Nest('c')), Nest('d')))
    // val nest: Nest[Char] = Nest('a', Nest(Nest('b','c'),'d'))
    // println(nest)

    // nest foreach println

    // val nest: List[Nest[Char]] = List(Nest('a'), Nest(Nest(Nest('b'), Nest('c')), Nest('d')))

    // println(nest)

    // nest foreach println


    // assert(nest == NestList('a', 'b'))
    assert(true)
  }

}
