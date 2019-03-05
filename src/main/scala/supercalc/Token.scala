package supercalc

sealed abstract class Token

class Paren extends Token

object ParenOpen extends Paren {
  override def toString = "ParenOpen"
}
object ParenClose extends Paren {
  override def toString = "ParenClose"
}

trait Command extends Token {
  val priority: Int
}

object CommandMultiple extends Command {
  val priority = 0
  override def toString = "CommandMultiple"
}
object CommandAdd extends Command {
  val priority = 1
  override def toString = "CommandAdd"
}

case class Number(num: Int) extends Token