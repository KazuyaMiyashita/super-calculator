package supercalc

import scala.util.Try

object Tokenizer {

  val numeric = new NumericCharHandler
  val command = new CommandCharHandler
  val paren = new ParenCharHandler
  val whiteSpace = new WhiteSpaceCharHandler

  numeric.setNext(command).setNext(paren).setNext(whiteSpace)

  /**
   * @param str ex. "1 add (22 multiple 333)"
   * @return List(Number(1), AddMultiple, ParenOpen, ...
   */
  def tokenize(str: String): Try[List[Token]] = Try {
    val (tokens, _) = handleAnything(Nil, str.toList)
    tokens.reverse
  }

  private def handleAnything(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    if (remain.isEmpty) (tokens, Nil)
    else {
      val (t, r) = numeric.support(tokens, remain)
      handleAnything(t, r)
    }
  }

}

sealed trait CharHandler {

  private var next: Option[CharHandler] = None
  def setNext(next: CharHandler): CharHandler = {
    this.next = Some(next)
    next
  }

  def support(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    if (remain.isEmpty) (tokens, Nil)
    else if (canHandle(remain.head)) handle(tokens, remain)
    else next match {
      case Some(n) => n.support(tokens, remain)
      case None => throw new Exception("Tokenize Error: No Support Char %s".format(remain.head))
    }
  }

  def canHandle(c: Char): Boolean
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char])

}

class NumericCharHandler extends CharHandler {

  def canHandle(c: Char) = '0' <= c && c <= '9'
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    handleMain(tokens, remain, Nil)
  }

  def constructToken(cs: List[Char]): Token = Number(cs.reverse.mkString.toInt)
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

class WhiteSpaceCharHandler extends CharHandler {

  def canHandle(c: Char) = c == ' '
  def handle(tokens: List[Token], remain: List[Char]): (List[Token], List[Char]) = {
    (tokens, remain.tail)
  }

}
