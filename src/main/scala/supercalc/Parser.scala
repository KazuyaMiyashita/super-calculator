package supercalc

import scala.util.{Try, Success, Failure}

object Parser {

  def parse(tokens: List[Token]): Option[BinaryTree[Token]] = {

    val nested: Option[List[Any]] = nest(tokens)
    val tree: Option[BinaryTree[Token]] = nested map { n => mkTree(n) }

    tree
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

  def mkTree(nest: List[Any]): BinaryTree[Token] = {

    // class TokenLike
    // case class TokenList extends List[Token] with TokenLike
    // case class TokenBinaryTree extends BinaryTree[Token] with TokenLike

    /**
     * tokensの長さが奇数 (ex. Number(1), CommandMultiple, Number(2), CommandAdd, Number(3) の時に
     * 計算順序を考慮して二分木にする
     */
    def convert(tokens: List[Token]): BinaryTree[Token] = {
      def proc(leaves: List[BinaryTree[Token]]): List[BinaryTree[Token]] = {
        if (leaves.length == 1) {
          if (leaves(0).isInstanceOf[Leaf[Number]]) leaves(0) :: Nil
          else throw new Exception("Commandのみのトークン")
        }
        else if (leaves.length == 3) {
          if (
            leaves(0).isInstanceOf[Leaf[Number]] &&
            leaves(1).isInstanceOf[Leaf[Command]] &&
            leaves(2).isInstanceOf[Leaf[Number]]
          ) Node(leaves(1).asInstanceOf[Leaf[Command]].value, leaves(0), leaves(2)) :: Nil
          else throw new Exception("Number Command Numberの順になっていない")
        }
        else {
          val highPriority = leaves.filter(_.isInstanceOf[Leaf[Command]]).minBy(_.asInstanceOf[Leaf[Command]].value.priority)
          val index = leaves.indexWhere(_ == highPriority)
          val tree: List[BinaryTree[Token]] = leaves.take(index-1) ::: proc(leaves.slice(index-1, index+2)) ::: leaves.take(index+2)
          tree
        }
      }

      if (tokens.length % 2 == 0) throw new Exception("トークンのリストを木にできない")
      else {
        val leaves: List[Leaf[Token]] = tokens.map { t => Leaf(t) }
        proc(leaves).head
      }
    }

    nest.map({ n => 
      if (n.isInstanceOf[List[Any]]) {
        val tree = mkTree(n.asInstanceOf[List[Any]])
        println(tree)
        tree
      }
      else n
    }).asInstanceOf[BinaryTree[Token]]
  }

}