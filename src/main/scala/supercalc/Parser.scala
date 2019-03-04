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

  /**
   * @params tokens: ex. List(Number(1), ParenOpen, Number(2), Number(3), ParenClose)
   * @return ex. List(Number(1), List(Number(2), Number(3))
   */
  def nest(tokens: List[Token]): List[Any] = {

    def proc(nest: List[Any], remain: List[Token], acc: List[Token]): (List[Any], List[Token], List[Token]) = {
      if (remain.isEmpty) (nest, Nil, Nil)
      else {
        remain.head match {
          case ParenOpen => {
            val (n, r, a) = proc(Nil, remain.tail, Nil)
            proc(n ::: nest, r, a)
          }
          case ParenClose => proc(nest :: Nil, remain.tail, Nil)
          case t => proc(t :: nest, remain.tail, Nil)
        }
      }
    }

    def reverse(nest: List[Any]): List[Any] = {
      nest.reverse.map { n => if (n.isInstanceOf[List[Any]]) reverse(n.asInstanceOf[List[Any]]) else n }
    }

    val (nest, _, _) = proc(Nil, tokens, Nil)
    reverse(nest)
  }

}