package supercalc

import scala.util.{Try, Success, Failure}

class Eval {
  def apply(str: String): Result = {
    val result: Result = (Tokenizer.tokenize(str) match {
      case Success(token) => Right(Parser.parse(token))
      case Failure(e) => Left(ErrorResult(e))
    }) flatMap {
      case Some(tokens) => Right(eval(tokens))
      case None => Left(ErrorResult(new Exception("Parse error")))
    } flatMap {
      case Some(result) => Right(CorrectResult(result))
      case None => Left(ErrorResult(new Exception("Eval error")))
    } merge

    result
  }

  def eval(tokens: BinaryTree[Token]): Option[String] = Some("hoge")
}
