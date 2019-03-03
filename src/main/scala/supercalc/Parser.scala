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
    def proc(nest: Nest[Token], remain: List[Token]): (Nest[Token], List[Token]) = {
      println("nest: %s, remain: %s".format(nest, remain))
      if (remain.isEmpty) (nest, Nil)
      else {
        remain.head match {
          case ParenOpen => {
            val (newNest, newRemain) = proc(nest, remain.tail)
            (NestList(nest, newNest), newRemain)
          }
          case ParenClose => (nest, remain.tail)
          case t => {
            nest match {
              case NestNil => proc(NestLeaf(t), remain.tail)
              case n => proc(NestList(n, NestLeaf(t)), remain.tail)
            }
          }
        }
      }
    }

    val (nest, remain) = proc(NestNil, tokens)
    nest
  }

}