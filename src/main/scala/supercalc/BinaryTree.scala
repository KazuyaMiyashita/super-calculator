package supercalc

class BinaryTree[+T]

case class Node[T](op: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]
case class Leaf[T](value: T) extends BinaryTree[T]