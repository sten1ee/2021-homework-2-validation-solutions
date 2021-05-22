package homework2

import homework2.DateValidation.validateDate

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort extends RegistrationFormError
case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError
case object PasswordsDoNotMatch extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError]) extends RegistrationFormError
case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

object UserValidation {
  val MinPasswordLength = 8

  def validateName(name: String): Validated[RegistrationFormError, String] = {
    if (name.nonEmpty) Valid(name)
    else Invalid(NameIsEmpty)
  }

  def validateEmail(email: String): Validated[RegistrationFormError, Email] = email match {
    case Email(name, domain) => Valid(Email(name, domain))
    case _ => Invalid(InvalidEmail(email))
  }

  def validatePassword(password: String, passwordConfirmation: String): Validated[RegistrationFormError, String] = {
    val validatedPasswordLength =
      if (password.length >= MinPasswordLength) Valid(password) else Invalid(PasswordTooShort)

    val validatedPasswordVariety = {
      val hasLetter = password.exists(_.isLetter)
      val hasDigit = password.exists(_.isDigit)
      val hasSpecialSymbol = password.exists(c => !c.isLetter && !c.isLetter)

      if (hasLetter && hasDigit && hasSpecialSymbol) Valid(password)
      else Invalid(PasswordRequiresGreaterSymbolVariety)
    }

    val validatedPasswordMatch = if (password == passwordConfirmation) Valid(password) else Invalid(PasswordsDoNotMatch)

    (
      validatedPasswordLength,
      validatedPasswordVariety,
      validatedPasswordMatch
    ).zip.map(_ => password)
  }

  def validateBirthday(today: Date)(
    year: String,
    month: String,
    day: String
  ): Validated[RegistrationFormError, Date] = {
    def validateBirthdayBeforeToday(date: Date) = {
      if (date <= today) Valid(date)
      else Invalid(BirthdayDateIsInTheFuture(date))
    }

    validateDate(year, month, day)
      .mapErrors(InvalidBirthdayDate)
      .flatMap(validateBirthdayBeforeToday)
  }

  def validatePostalCode(userCountryPostalCodeVerifier: String => Boolean)(postalCode: String)
    : Validated[RegistrationFormError, Option[String]] = {
    if (postalCode.isEmpty) Valid(None)
    else if (userCountryPostalCodeVerifier(postalCode)) Valid(Some(postalCode))
    else Invalid(InvalidPostalCode(postalCode))
  }
}
