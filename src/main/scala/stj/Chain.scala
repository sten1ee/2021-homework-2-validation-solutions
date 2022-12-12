package stj

sealed trait Chain[+A] {
  def head: A

  def tail: Chain[A] = this match {
    case Singleton(_) => throw new NoSuchElementException()
    case Append(Singleton(_), tl) => tl
    case Append(_, _) => listify.tail
  }

  def isEmpty: Boolean = false

  def listify: Chain[A] =
    this match {
      case Singleton(_) => this
      case Append(left, right) => left.listifyEx(right.listify)
    }

  protected def listifyEx[B >: A](listifiedTail: Chain[B]): Chain[B] =
    this match {
      case Singleton(_) => Append(this, listifiedTail)
      case Append(left, right) => left.listifyEx(right.listifyEx(listifiedTail))
    }

  // добавяне на елемент в началото
  def +:[B >: A](front: B): Chain[B] = Append(Singleton(front), this)

  // добавяне на елемент в края
  def :+[B >: A](back: B): Chain[B] = Append(this, Singleton(back))

  // слепване две последователности (Chain(1, 2) ++ Chain(3, 4) == Chain(1, 2, 3, 4))
  def ++[B >: A](otherChain: Chain[B]): Chain[B] = Append(this, otherChain)

  //Операции foldLeft, map, flatMap със вече познатата ни семантика
}

case class Singleton[+A](head: A) extends Chain[A]
case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  override def head: A = left.head
}