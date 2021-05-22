package homework2

case class RegistrationForm(
  name: String,
  email: String,
  password: String,
  passwordConfirmation: String,
  birthYear: String,
  birthMonth: String,
  birthDay: String,
  postalCode: String
)

case class Email(user: String, domain: String)

object Email {
  def unapply(email: String): Option[(String, String)] = email.split("@") match {
    case Array(name, domain) if name.nonEmpty && domain.nonEmpty => Some((name, domain))
    case _ => None
  }
}

case class User(
  name: String,
  email: Email,
  passwordHash: String,
  birthday: Date,
  postalCode: Option[String]
)

object UserRegistration {
  import UserValidation._

  def registerUser(
    userCountryPostalCodeVerifier: String => Boolean,
    today: Date
  )(form: RegistrationForm): Validated[RegistrationFormError, User] = {
    (
      validateName(form.name),
      validateEmail(form.email),
      validatePassword(form.password, form.passwordConfirmation).map(PasswordUtils.hash),
      validateBirthday(today)(form.birthYear, form.birthMonth, form.birthDay),
      validatePostalCode(userCountryPostalCodeVerifier)(form.postalCode)
    ).zipMap(User.apply)
  }
}
