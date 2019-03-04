package supercalc.tokenize

object Tokenizer {

  val numeric = new NumericCharHandler
  val command = new CommandCharHandler
  val paren = new ParenCharHandler
  val whiteSpace = new WhiteSpaceCharHandler

  numeric.setNext(command).setNext(paren).setNext(whiteSpace)

  /**
   * @param str ex. "1 add (22 multiple 333)"
   * @return List(Number(1), CommandMultiple, ParenOpen, ...
   */
  def tokenize(str: String): List[Token] = {
    val (tokens, _) = handleAnything(Nil, str.toList)
    tokens.reverse
  }

  private def handleAnything(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    println("handleAnything: > tokens: %s, remain: %s".format(tokens, remain))
    if (remain.isEmpty) (tokens, Nil)
    else {
      val (t, r) = numeric.support(tokens, remain)
      handleAnything(t, r)
    }
  }

}