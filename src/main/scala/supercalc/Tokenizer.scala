package supercalc

object Tokenizer {

  /**
   * @param str ex. "1 add (22 multiple 333)"
   * @return List(Number(1), CommandMultiple, ParenOpen, ...
   */
  def tokenize(str: String): List[Token] = {
    val (tokens, _) = handleAnything(Nil, str.toList)
    tokens.reverse
  }

  private def handleAnything(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    // println("handleAnything: > tokens: %s, remain: %s".format(tokens, remain))
    if (remain.isEmpty) (tokens, Nil)
    else {
      val (newTokens, newRemain) = remain.head match {
        case c if isNumericChar(c) => handleNumericChar(tokens, remain, Nil)
        case c if isCommandChar(c) => handleCommandChar(tokens, remain, Nil)
        case c if isParenChar(c) => handleParenChar(tokens, remain)
        case ' ' => (tokens, remain.tail)
        case c => throw new Exception("Parse Error: No Existing Literal %s".format(c))
      }
      handleAnything(newTokens, newRemain)
    }
  }

  private def isNumericChar(c: Char) = '0' <= c && c <= '9'
  private def constructNumberToken(cs: List[Char]): Token = Number(cs.reverse.mkString.toInt)
  private def handleNumericChar(tokens: List[Token], remain: List[Char], acc: List[Char]): (List[Token], List[Char]) = {
    // println("handleNumericChar > tokens: %s, remain: %s, acc: %s".format(tokens, remain, acc))
    if (remain.isEmpty) (tokens, Nil)
    else {
      remain.head match {
        case c if isNumericChar(c) => handleNumericChar(tokens, remain.tail, c :: acc)
        case _ => (constructNumberToken(acc) :: tokens, remain)
      }
    }
  }

  private def isCommandChar(c: Char) = 'a' <= c && c <= 'z'
  private def constructCommandToken(cs: List[Char]): Token = cs.reverse.mkString match {
    case "add" => CommandAdd
    case "multiple" => CommandMultiple
    case s => throw new Exception("Parse Error: No Existing Command %s".format(s))
  }
  private def handleCommandChar(tokens: List[Token], remain: List[Char], acc: List[Char]): (List[Token], List[Char]) = {
    // println("handleCommandChar >tokens: %s, remain: %s, acc: %s".format(tokens, remain, acc))
    if (remain.isEmpty) (tokens, Nil)
    else {
      remain.head match {
        case c if isCommandChar(c) => handleCommandChar(tokens, remain.tail, c :: acc)
        case _ => (constructCommandToken(acc) :: tokens, remain)
      }
    }
  }

  private def isParenChar(c: Char) = c == '(' || c == ')'
  private def handleParenChar(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    // println("handleParenChar: > tokens: %s, remain: %s".format(tokens, remain))
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