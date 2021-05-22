package homework2

sealed trait Validated[+E, +A] {
  def isValid: Boolean = fold(_ => false)(_ => true)

  def getOrElse[B >: A](default: => B): B = fold(_ => default)(identity)

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = fold(_ => default)(_ => this)

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = (this, vb) match {
    case (Valid(a), Valid(b)) => Valid((a, b))
    case (Invalid(errors1), Invalid(errors2)) => Invalid(errors1 ++ errors2)
    case (i @ Invalid(_), _) => i
    case (_, i @ Invalid(_)) => i
  }

  def map[B](f: A => B): Validated[E, B] = flatMap(a => Valid(f(a)))

  def mapErrors[E2](f: Chain[E] => E2): Validated[E2, A] =
    fold[Validated[E2, A]](errors => Invalid(f(errors)))(a => Valid(a))

  def zipMap[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = zip(vb).map(f.tupled)

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    fold[Validated[EE, B]](errors => Invalid(errors))(a => f(a))

  def fold[B](fInvalid: Chain[E] => B)(f: A => B): B = this match {
    case Valid(value) => f(value)
    case Invalid(errors) => fInvalid(errors)
  }

  def foreach(f: A => Unit): Unit = fold(_ => ())(f)
}

case class Valid[+A](value: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  implicit class ValidatedTuple2[EE, A, B](
    val tuple: (
      Validated[EE, A],
      Validated[EE, B]
    )
  ) extends AnyVal {
    def zip: Validated[EE, (A, B)] = tuple match {
      case (va, vb) => va.zip(vb)
    }
    def zipMap[R](f: (A, B) => R): Validated[EE, R] = tuple.zip.map(f.tupled)
  }

  implicit class ValidatedTuple3[EE, A, B, C](
    val tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C]
    )
  ) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = {
      val (va, vb, vc) = tuple
      (va zip vb zip vc).map { case ((a, b), c) => (a, b, c) }
    }
    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = tuple.zip.map(f.tupled)
  }

  implicit class ValidatedTuple4[EE, A, B, C, D](
    val tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D]
    )
  ) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = {
      val (va, vb, vc, vd) = tuple
      (va zip vb zip vc zip vd).map { case (((a, b), c), d) => (a, b, c, d) }
    }
    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = tuple.zip.map(f.tupled)
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E](
    val tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D],
      Validated[EE, E]
    )
  ) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = {
      val (va, vb, vc, vd, ve) = tuple
      (va zip vb zip vc zip vd zip ve).map { case ((((a, b), c), d), e) => (a, b, c, d, e) }
    }
    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = tuple.zip.map(f.tupled)
  }

  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = {
    xs.foldRight(Valid(List.empty): Validated[E, List[A]])((next, acc) => (next, acc).zipMap(_ :: _))
  }

  implicit class ValidatedOptionOps[A](val o: Option[A]) extends AnyVal {
    def toValidated[E](error: E): Validated[E, A] = o.map(Valid(_)).getOrElse(Invalid(error))
  }
}
