package supercalc

sealed trait Result

case class CorrectResult(result: String) extends Result
case class ErrorResult(e: Throwable) extends Result


