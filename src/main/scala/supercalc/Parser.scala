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

}