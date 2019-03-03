package supercalc

object Parser {

  def parse(tokens: List[Token]): BinaryTree[Token] = {
    Node(
      op = CommandAdd,
      left = Leaf(Number(1)),
      right = Node(
        op = CommandMultiple,
        left = Leaf(Number(23)),
        right = Leaf(Number(456))
      )
    )
  }

  // def handleNumberToken(tokens: List[Token]): BinaryTree[Token] = {
  //   if (tokens.length <= 1) Leaf(tokens.head)
  //   else {
  //     tokens.head match {
  //       case t: Command => 
  //     }
  //   }
  // }

  def nest(tokens: List[Token]): Nest[Token] = {
    def proc(nest: Nest[Token], remain: List[Token], acc: Nest[Token]): (Nest[Token], List[Token], Nest[Token]) = {
      if (remain.isEmpty) (nest, remain, NestNil)
      else {
        remain.head match {
          case ParenOpen => proc(nest, remain.tail, NestLeaf(remain.head))
          case ParenClose => proc(List(NestList(acc.value.reverse), nest), remain.tail, NestNil)
          case t => proc(NestLeaf(t), remain.tail, acc)
        }
      }
    }

    ???

  }

}