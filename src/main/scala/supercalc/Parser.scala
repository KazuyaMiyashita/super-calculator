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

    def proc(nest: List[Any], remain: List[Token]): (List[Any], List[Token]) = {
      if (remain.isEmpty) (nest, Nil)
      else {
        remain.head match {
          case ParenOpen => {
            val (n, r) = proc(Nil, remain.tail)
            proc(n ::: nest, r)
          }
          case ParenClose => proc(nest :: Nil, remain.tail)
          case t => proc(t :: nest, remain.tail)
        }
      }
    }

    def reverse(nest: List[Any]): List[Any] = {
      nest.reverse.map { n => if (n.isInstanceOf[List[Any]]) reverse(n.asInstanceOf[List[Any]]) else n }
    }

    val (nest, _) = proc(Nil, tokens)
    reverse(nest)
  }

}