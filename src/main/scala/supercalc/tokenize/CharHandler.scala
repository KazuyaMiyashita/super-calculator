package supercalc.tokenize

class CharHandler {

  private var next: Option[CharHandler] = None
  def setNext(next: CharHandler): CharHandler = {
    this.next = Some(next)
    next
  }

  def support(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    if (remain.isEmpty) (tokens, Nil)
    else if (canHandle(remain.head)) handle(tokens, remain)
    else next match {
      case Some(n) => n.handle(tokens, remain)
      case None => throw new Exception("Parse Error: No Support Char %s".format(remain.head))
    }
  }

  def canHandle(c: Char): Boolean = false
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char])

}