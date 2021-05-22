package homework2

import homework2.Validated.ValidatedOptionOps

import scala.util.Try

sealed trait DateError
case class YearIsNotAnInteger(year: String) extends DateError
case class MonthIsNotAnInteger(month: String) extends DateError
case class DayIsNotAnInteger(day: String) extends DateError
case class MonthOutOfRange(month: Int) extends DateError
case class DayOutOfRange(day: Int) extends DateError
case class InvalidDate(year: Int, month: Int, day: Int) extends DateError

object DateValidation {
  // These two can probably go in some other reusable util
  private def toInteger(n: String): Option[Int] = Try(n.toInt).toOption

  private def validateInRange[E](min: Int, max: Int)(error: E)(n: Int): Validated[E, Int] =
    if (min <= n && n <= max) Valid(n)
    else Invalid(error)

  def validateDate(year: String, month: String, day: String): Validated[DateError, Date] = {
    val validatedYear = toInteger(year).toValidated(YearIsNotAnInteger(year))
    val validatedMonth = for {
      m <- toInteger(month).toValidated(MonthIsNotAnInteger(month))
      _ <- validateInRange(1, 12)(MonthOutOfRange(m))(m)
    } yield m
    val validatedDay = for {
      d <- toInteger(day).toValidated(DayIsNotAnInteger(day))
      _ <- validateInRange(1, 31)(DayOutOfRange(d))(d)
    } yield d

    (
      validatedYear,
      validatedMonth,
      validatedDay
    ).zip.flatMap {
      case (y, m, d) => Date.applyOption(y, m, d).toValidated(InvalidDate(y, m, d))
    }
  }
}
