package supercalc.tokenize

class WhiteSpaceCharHandler extends CharHandler {

  def canHandle(c: Char) = c == ' '
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    (tokens, remain.tail)
  }

}