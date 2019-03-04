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

  // def nest(tokens: List[Token]): Nest[Token] = {
  //   def proc(nest: Nest[Token], remain: List[Token]): (Nest[Token], List[Token]) = {
  //     println("nest: %s, remain: %s".format(nest, remain))
  //     if (remain.isEmpty) (nest, Nil)
  //     else {
  //       remain.head match {
  //         case ParenOpen => {
  //           val (newNest, newRemain) = proc(nest, remain.tail)
  //           (NestList(nest, newNest), newRemain)
  //         }
  //         case ParenClose => (nest, remain.tail)
  //         case t => {
  //           nest match {
  //             case NestNil => proc(NestLeaf(t), remain.tail)
  //             case n => proc(NestList(n, NestLeaf(t)), remain.tail)
  //           }
  //         }
  //       }
  //     }
  //   }

  // def nest(tokens: List[Token]): List[Any] = {
  //   def proc(nest: List[Any], remain: List[Token], acc: List[Token]): (List[Any], List[Token], List[Token]) = {
  //     println("nest: %s, remain: %s".format(nest, remain))
  //     if (remain.isEmpty) (nest.reverse, Nil)
  //     else {
  //       remain.head match {
  //         case ParenOpen => {
  //           val (newNest, newRemain) = proc(nest, remain.tail)
  //           (List(newNest) :: nest, newRemain)
  //         }
  //         case ParenClose => (nest.reverse, remain.tail)
  //         case t => proc(t :: nest, remain.tail)
  //       }
  //     }
  //   }

  //   val (nest, remain) = proc(Nil, tokens)
  //   nest
  // }

  // def nest(tokens: List[Token]): List[Any] = {

  //   def proc(nest: List[Any], remain: List[Token], acc: List[Token]): (List[Any], List[Token], List[Token]) = {
  //     println("nest: %s\nremain: %s\nacc: %s\n".format(nest, remain, acc))
  //     if (remain.isEmpty) (nest, Nil, Nil)
  //     else {
  //       remain.head match {
  //         case ParenOpen => proc(acc ::: nest, remain.tail, Nil)
  //         case ParenClose => proc(acc :: nest, remain.tail, Nil)
  //         case t => proc(nest, remain.tail, t :: acc)
  //       }
  //     }
  //   }

  //   def reverse(nest: List[Any]): List[Any] = {
  //     nest.reverse.map { n => if (n.isInstanceOf[List[Any]]) reverse(n.asInstanceOf[List[Any]]) else n }
  //   }

  //   val (nest, _, _) = proc(Nil, tokens, Nil)
  //   reverse(nest)
  // }

  /**
   * @params tokens: ex. List(Number(1), ParenOpen, Number(2), Number(3), ParenClose)
   * @return ex. List(Number(1), List(Number(2) ,Number(3))
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