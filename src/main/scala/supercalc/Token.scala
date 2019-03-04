package supercalc

sealed abstract class Token

class Paren extends Token

object ParenOpen extends Paren {
  override def toString = "ParenOpen"
}
object ParenClose extends Paren {
  override def toString = "ParenClose"
}

class Command extends Token

object CommandAdd extends Command {
  override def toString = "CommandAdd"
}
object CommandMultiple extends Command {
  override def toString = "CommandMultiple"
}

case class Number(num: Int) extends Token