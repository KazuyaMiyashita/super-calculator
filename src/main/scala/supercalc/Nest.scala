package supercalc

trait Nest[+T]

case object NestNil extends Nest[Nothing]
case class NestLeaf[T](value: T) extends Nest[T]
case class NestList[T](value: Nest[T]*) extends Nest[T]
