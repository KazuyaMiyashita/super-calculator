package supercalc

import scala.util.{Try, Success, Failure}

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
   * @param tokens ex. List(Number(1), ParenOpen, Number(2), Number(3), ParenClose)
   * @return ex. List(Number(1), List(Number(2), Number(3))
   */
  def nest(tokens: List[Token]): Option[List[Any]] = {

    def proc(nest: List[Any], remain: List[Token], indent: Int): (List[Any], List[Token], Int) = {
      if (remain.isEmpty) {
        if (indent == 0) (nest, Nil, 0)
        else throw new Exception("括弧の対応がおかしい")
      } else {
        remain.head match {
          case ParenOpen => {
            val (n, r, i) = proc(Nil, remain.tail, indent + 1)
            proc(n ::: nest, r, i)
          }
          case ParenClose => {
            if (indent > 0) proc(nest :: Nil, remain.tail, indent - 1)
            else throw new Exception("括弧の対応がおかしい")
          }
          case t => proc(t :: nest, remain.tail, indent)
        }
      }
    }

    def reverse(nest: List[Any]): List[Any] = {
      nest.reverse.map { n => if (n.isInstanceOf[List[Any]]) reverse(n.asInstanceOf[List[Any]]) else n }
    }

    Try {
      val (nest, _, _) = proc(Nil, tokens, 0)
      nest
    } match {
      case Success(n) => Some(reverse(n))
      case Failure(e) => None
    }
  }

}