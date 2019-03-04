package supercalc

class ParenCharHandler extends CharHandler {

  def canHandle(c: Char) = c == '(' || c == ')'
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    if (remain.isEmpty) (tokens, Nil)
    else {
      remain.head match {
        case '(' => (ParenOpen :: tokens, remain.tail)
        case ')' => (ParenClose :: tokens, remain.tail)
        case _ => throw new Exception("incorrect calling handleParenChar")
      }
    }
  }

}