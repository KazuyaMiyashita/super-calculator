package supercalc

sealed trait Nest[+T]

case object NestNil extends Nest[Nothing]
case class NestLeaf[T](value: T) extends Nest[T]
case class NestList[T](value: Nest[T]*) extends Nest[T] {
  override def toString = value.mkString("NestList(", ", ", ")")
}

// type Not[A] = A => Nothing
// type Or[A, B] = Not[Not[A] with Not[B]]

// case object NestNil extends Nest[Nothing]
// case class NestLeaf[T](value: T) extends Nest[T]
// case class NestList[T, A](value: A*)(implicit ev: Not[Not[A]] <:< Or[T, Nest[T]]) extends Nest[T] {
//   override def toString = value.mkString("NestList(", ", ", ")")
// }

object Nest {
  def apply() = NestNil
  def apply[T](value: T*) = {
    if (value.length <= 1) NestLeaf(value(0))
    else NestList(value.map({v => NestLeaf(v)}): _*)
  }
  def apply[T](value: Nest[T]*) = NestList(value: _*)
}
