package stj

import scala.annotation.tailrec

sealed trait Chain[+A] {
  def head: A

  def tail: Chain[A] = this match {
    case Singleton(_) => throw new UnsupportedOperationException("tail invoked on Singleton Chain")
    case Append(Singleton(_), tl) => tl
    case Append(_, _) => listify.tail
  }

  def isEmpty: Boolean = false

  def listify: Chain[A] =
    this match {
      case Singleton(_) => this
      case Append(left, right) => left.listifyEx(right.listify)
    }

  private def listifyEx[B >: A](trailingTail: Chain[B]): Chain[B] =
    this match {
      case Singleton(_) => Append(this, trailingTail)
      case Append(left, right) => left.listifyEx(right.listifyEx(trailingTail))
    }

  // добавяне на елемент в началото
  def +:[B >: A](front: B): Chain[B] = Append(Singleton(front), this) // chain +: el

  // добавяне на елемент в края
  def :+[B >: A](back: B): Chain[B] = Append(this, Singleton(back))

  // слепване две последователности (Chain(1, 2) ++ Chain(3, 4) == Chain(1, 2, 3, 4))
  def ++[B >: A](otherChain: Chain[B]): Chain[B] = Append(this, otherChain)

  def foldLeft[ACC, B >: A](acc: ACC)(f: (ACC, B) => ACC): ACC = this match {
    case Singleton(head) => f(acc, head)
    case Append(Singleton(leftValue), right) => right.foldLeft(f(acc, leftValue))(f)
    case Append(left, right) => right.foldLeft(left.foldLeft(acc)(f))(f)
  }

  def foldRight[B >: A, ACC](acc: ACC)(f: (B, ACC) => ACC): ACC = this match {
    case Singleton(head) => f(head, acc)
    case Append(left, Singleton(rightValue)) => left.foldRight(f(rightValue, acc))(f)
    case Append(left, right) => left.foldRight(right.foldRight(acc)(f))(f)
  }

  def map[B](f: A => B): Chain[B] = this match {
    case Singleton(head) => Singleton(f(head))
    case Append(left, right) => Append(left.map(f), right.map(f))
  }

  def flatMap[B](f: (A => Chain[B])): Chain[B] = this match {
    case Singleton(head) => f(head)
    case Append(left, right) => Append(left.flatMap(f), right.flatMap(f))
  }
}

case class Singleton[+A](head: A) extends Chain[A]
case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  override def head: A = left.head
}