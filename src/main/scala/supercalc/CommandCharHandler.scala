package supercalc

class CommandCharHandler extends CharHandler {

  def canHandle(c: Char) = 'a' <= c && c <= 'z'
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    handleMain(tokens, remain, Nil)
  }

  def constructToken(cs: List[Char]): Token = cs.reverse.mkString match {
    case "add" => CommandAdd
    case "multiple" => CommandMultiple
    case s => throw new Exception("Parse Error: No Existing Command %s".format(s))
  }
  def handleMain(tokens: List[Token], remain: List[Char], acc: List[Char]): (List[Token], List[Char]) = {
    if (remain.isEmpty) (tokens, Nil)
    else {
      remain.head match {
        case c if canHandle(c) => handleMain(tokens, remain.tail, c :: acc)
        case _ => (constructToken(acc) :: tokens, remain)
      }
    }
  }

}