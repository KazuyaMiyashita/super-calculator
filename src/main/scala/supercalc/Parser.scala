package supercalc

import scala.util.{Try, Success, Failure}

object Parser {

  def parse(tokens: List[Token]): Option[BinaryTree[Token]] = {

    val nested: Try[Nest[Token]] = Try {
      Nest.group(tokens, ParenOpen, ParenClose)
    }
    val tree: Option[BinaryTree[Token]] = nested map { n => mkTree(n) }

    tree
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
