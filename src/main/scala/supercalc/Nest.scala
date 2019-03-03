package supercalc

sealed trait Nest[+T]

case object NestNil extends Nest[Nothing]
case class NestLeaf[T](value: T) extends Nest[T]
case class NestList[T](value: Nest[T]*) extends Nest[T] {
  override def toString = value.mkString("NestList(", ", ", ")")
}

object Nest {
  def apply() = NestNil
  def apply[T](value: T) = NestLeaf(value)
  def apply[T](value: Nest[T]*) = NestList(value: _*)
}
