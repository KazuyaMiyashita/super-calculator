package supercalc

sealed trait Nest[+T] {
  def reverse: Nest[T]
}

case object NestNil extends Nest[Nothing] {
  def reverse: Nest[Nothing] = this
}
case class NestLeaf[T](code: T) extends Nest[T] {
  def reverse: Nest[T] = this
}
case class NestList[T](elements: Nest[T]*) extends Nest[T] {
  def reverse: Nest[T] = NestList(elements.reverse.map(_.reverse):_*)
  override def toString = elements.mkString("NestList(", ", ", ")")
}

object Nest {

  def apply() = NestNil
  def apply[T](value: T*) = {
    if (value.length <= 1) NestLeaf(value(0))
    else NestList(value.map({v => NestLeaf(v)}): _*)
  }
  def apply[T](value: Nest[T]*) = NestList(value: _*)

  def group[T](tokens: List[T], parenOpen: T, parenClose: T): Nest[T] = {
    def process(nest: List[Nest[T]], acc: List[Nest[T]], remain: List[T]): (List[Nest[T]], List[Nest[T]], List[T]) = {
      remain match {
        case Nil => (nest, Nil, Nil)
        case `parenOpen`::rest =>
          val (n, a, r) = process(Nil, nest, rest)
          process(n ::: acc, a, r)
        case `parenClose`::rest =>
          process(NestList(nest:_*) :: Nil ::: acc, Nil, rest)
        case t::rest =>
          process(NestLeaf(t) :: nest, acc, rest)
      }
    }
  
    val (nest, _, _) = process(Nil, Nil, tokens)
    Nest(nest.reverse:_*)
  }

}
