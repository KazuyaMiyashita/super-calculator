package supercalc.tokenize

class NumericCharHandler extends CharHandler {

  def canHandle(c: Char) = '0' <= c && c <= '9'
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    handleMain(tokens, remain, Nil)
  }

  def constructToken(cs: List[Char]): Token = Number(cs.reverse.mkString.toInt)
  def handleMain(tokens: List[Token], remain: List[Char], acc: List[Char]): (List[Token], List[Char]) = {
    println("handleNumericChar > tokens: %s, remain: %s, acc: %s".format(tokens, remain, acc))
    if (remain.isEmpty) (tokens, Nil)
    else {
      remain.head match {
        case c if canHandle(c) => handleMain(tokens, remain.tail, c :: acc)
        case _ => (constructToken(acc) :: tokens, remain)
      }
    }
  }

}