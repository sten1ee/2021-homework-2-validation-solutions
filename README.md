краен срок: 09.05.2021
---
# Валидация

В това домашно ще имплементираме малка библиотека за валидация и ще я използваме в програмка за валидация на регистрационна форма.

Целта на домашното е да видим как темите и езиковите средства, за които говорихме последните няколко лекции, работят заедно и да се упражним в изграждането на композитна абстракция чрез тях.

## Непразни последователности. `Chain` (2 точки)

В курса разгледахме как тоталните функции ни помагат да се предпазим от неочаквани ситуации. За да постигнем това, често разширяваме резултатния тип за да опишем тези гранични ситуации – например връщаме `Option`, `Try`, `Either` или нещо друго.

Това обаче може да направи работата за клиентите на тези функции по неудобна, например:

```scala
def average(numbers: List[Double]): Option[Double] =
  if (numbers.isEmpty) None
  else Some(numbers.sum / numbers.size)

average(List(2, 3, 7)).getOrElse(sys.error("Unexpected"))
``` 

В случая клиентът знае, че списъкът не е празен и притежава средно аритметично, но типовата система по никакъв начин не му помага за това и му налага да се справи и със случая на подаден празен списък. Понякога в такива ситуации е възможно да елиминираме тези невъзможни състояния, така че да не е необходимо да се грижим за тях, ограничавайки наборът от възможните входни аргументи на функциите. Ако вместо `List` приемем тип `NonEmptyList`, то функцията би изглеждала така:

 ```scala
 def average(numbers: NonEmptyList[Double]): Double = numbers.sum / numbers.size
 
 average(NonEmptyList(2, 3, 7))
 ```

Това много по-ясно и директно описва проблемът, който решаваме.

Първата част от това домашно е да имплементираме такава непразна последователност. Вместо чрез стандартен свързан списък обаче, ще извършим имплементацията чрез структура, оптимизирана за друг тип операции – слепване на последователности. Ще наричаме този тип `Chain`. Ще искаме слепването на две `Chain` инстанции да е с константна сложност, за сметка на други стандартни операции, като извличане на първи елемент и достъп до остатъка от верига, които ще се наложи да се превърнат в линейни в най-лошия случай.

Доста често във функционални трансформации ни се налага да слепваме междинни последователности, които получаваме, след което линейно да обходим и трансформираме целия резултат. Именно в тези случаи би бил полезен `Chain`.

Дефинираме `Chain` като сума на два подтипа: верига от един елемент (`Singleton`) и слепване на две вериги (`Append`):

```scala
sealed trait Chain[+A]
case class Singleton[+A](head: A) extends Chain[A]
case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A]
```

В предоставения ви код някои от операциите, като `head`, `tail` и `isEmpty`, са вече реализирани за вас. Вашата задача е да допълните имплементация със следните операции за `Chain`:

* `def +:[B >: A](front: B): Chain[B]` – добавяне на елемент в началото
* `def :+[B >: A](back: B): Chain[B]` – добавяне на елемент в края
* `def ++[B >: A](right: Chain[B]): Chain[B]` – слепване две последователности (`Chain(1, 2) ++ Chain(3, 4) == Chain(1, 2, 3, 4)`)
* Операции `foldLeft`, `map`, `flatMap` със вече познатата ни семантика
* `def listify: Chain[A]`, която връща нова верига с променена структура, но стойностно равняваща се на оригиналната последователност. Променената структура наподобява структурата на свързания списък и изискването е всеки ляв елемент на `Append` верига да бъде `Singleton`.

  Например 
  
  ```scala
  Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).listify
  ```
  
  връща 
  
  ```scala
  Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))
  ```
  
  При тази структура на веригата операциите `head` и `tail` стават константни.
* `def apply[A](head: A, rest: A*): Chain[A]` върху companion обекта `Chain` – създава верига с първи елемент `head` и последователността `rest` за оставащите елементи.

Автоматично генерираните от `case` класовете методи `equals` и `hashCode` в този случай няма да са ни полезни, тъй като две вериги с различна структура, но със същата последователност от елементи, също трябва да бъдат равни. Затова предоставяме собствена имплементация и на тези методи.

В предоставения ви код е добавена имплементация на `hashCode`, която добре разпределя възможните върнати стойности (възползвайки се от свойствата на простите числа, в случая 31). От вас се изисква да имплементирате метода `equals`.

0,5(__*__) от 2-те точки за тази задача са с леко повишена трудност и ще ви ги дадем, ако имплементирате `foldLeft` опашково-рекурсивно. Останалите операции не е нужно да са опашково-рекурсивни, но, ако ви е любопитно, е добро упражнение да помислите как и дали биха могли да станат.

## `Validated` (2 точки)

На лекциите разгледахме няколко типа, енкапсулиращи възможни грешки или успешна стойност – `Option`, `Try` и `Either`. Както видяхме, ефектните трансформации, включващи тези типове, имат свойството, че прекъсват при първата срещната грешка. Например:

```scala
for {
  name <- validateName(form.name)
  email <- validateEmail(form.email)
  password <- validatePassword(form.password)
} yield User(name, email, password)
```

Ако името е невалидно, то въобще няма да се направи опит за валидация на email или парола. Това е поведението на `flatMap` операцията и то е изключително полезно за ранно прекъсване на трансформации и избягване на ненужни или невъзможни изчисления.

В някои случаи обаче бихме искали тези изчисления да бъдат извършени, ако това е смислено. Например в горния случай може би искаме да съобщим на потребителя не само за първата грешка, която е допуснал при попълването на регистрационния формуляр, ами вместо това за всички, за да получи бърза обратна връзка и да ги поправи наведнъж.

За целта ни е нужна нова операция и подходящ тип, който да събира всички натрупани грешки, вместо да пази само една. Този тип ще наречем `Validated`:

```scala
sealed trait Validated[+E, +A]
case class Valid[+A](value: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]
```

Грешките от тип `E` ще запазваме в непразна верига. В случай, че нямаме нито една грешка, то получаваме валидна стойност от тип `A`. Операцията, която ни позволява да комбинираме две независими валидации и да съберем грешките от тях, ще наречем `zip`. Имплементирайте следните операции:

* `def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)]` – комбинира два валидни резултата в двойка `(A, B)`. При наличие на грешки вместо това събира всичките в инстанция от тип `Invalid[EE]`
  
  ```scala
  Valid(1) zip Valid("a") // Valid((1, "a"))
  
  Invalid(Chain(1)) zip Valid("a") // Invalid(Chain(1))
  
  Invalid(Chain(1)) zip Invalid(Chain(2, 3)) // Invalid(Chain(1, 2, 3))
  ```
* `def isValid: Boolean` – проверява дали инстанцията репрезентира валидна стойност (а не грешки)
* `def getOrElse[B >: A](default: => B): B` – връща валидната стойност, ако е налична, или `default`, в противен случай
* `def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B]` – връща `this`, ако инстанцията съдържа валидна стойност, или `default` в противен случай
* Операции `map` и `flatMap` – познатите ни операции. `flatMap` има прекъсващото поведение, което споменахме по-горе
* `def zipMap[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R]` – операция, производна на `zip` – комбинира два валидни резултата и ги подава на функция `f`. Резултатът е върнатата стойност на `f`, обвита във `Valid`. При наличие на грешки, то функцията се държи като `zip`.
  
  ```scala
  Valid(1).zipMap(Valid(2))(_ + _) // Valid(3)

  Valid(1).zipMap(Invalid(Chain(42)))(_ + _) // Invalid(Chain(42))
  ```
* `def fold[B](fInvalid: Chain[E] => B)(f: A => B): B` – ако инстанцията съдържа валидна стойност връща `f(value)`, в противен случай връща `fInvalid(errors)`. Тази функция ни позволява лесно да генерираме стойност и от двата случая.

Допълнително, ще искаме да имплементираме няколко помощни класове и функции към съпътстващия обект на `Validated`:

* Горният `zip` работи само с двойки. Би било полезно да си създадем DSL, работещ с произволни n-торки. За целта ще използваме extension методи, за да обогатим всички n-торки от `Validated` инстанции. Това би била например двойката `(Valid(1), Invalid(Chain(3)))`. Методите, които ще добавим, са следните:
  * `def zip: Validated[EE, (A, B, ..., N)]` – комбинира стойностите на инстанциите в една n-торка
    
    ```scala
    (Valid(1), Valid("2"), Valid(3.0)).zip // Valid((1, "2", 3.0)): Validated[Nothing, (Int, String, Double)]
    (Valid(1), Invalid(Chain(3))).zip // Invalid(Chain(3)): Validated[Int, (Int, Nothing)]
    ```
    
  * `def zipMap[R](f: (A, B, .., N) => R): Validated[EE, R]` – подобно на `zipMap` функцията върху `Validated` обекти, само че за n инстанции
  
  За съжаление Scala 2 не ни предоставя начин да генерилизираме рекурентно по броя типови параметри на n-торките/функциите. Затова ще е нужно да предоставим имплементации за всеки един тип от `Tuple2` до `Tuple22`. В това домашно ще искаме от вас да добавите имплементации до `Tuple5` (`Tuple5` ще ни е полезна при имплементация на третата част от задачата).
  
  Чрез този DSL можем да направим следното:
  
  ```scala
  (
    validateName(form.name),
    validateEmail(form.email),
    validatePassword(form.password)
  ).zipMap(User.apply)
  ```
  
  Тук създаваме тройка (`Tuple3`) от `Validated` инстанции, които комбинираме чрез `zipMap` и подаваме на конструктора на `User`.
* Чрез подобно обогатяване, добавете метод `def toValidated[E](onEmpty: => E): Validated[E, A]` към инстанциите от тип `Option[A]` 
* (__*__) `def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]]` – подобно на `zip`, но вместо това работи със списък. Комбинира списъка от всички подадени `Validated` инстанции в една `Validated` инстанция, съдържаща списък от всичките им стойности. В случай, че някоя от `Validated` инстанциите съдържа грешки, то резултатът е `Invalid[E]` с грешките от всички невалидни инстанции.

## Валидация на регистрационна форма (3 точки)

След като имплементирахме нашата библиотечка за валидация, нека да я използваме на практика.

В уеб програмирането често получаваме формуляри под формата на съвкупност от низове, които потребителят е въвел. Тук ще имитираме такъв формуляр за регистрация, като целта ни е при грешки да уведомим потребителя за всички от тях наведнъж.

Входът от регистрационната форма моделираме по следния начин:

```scala
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
```

Потребителят въвежда всяко едно от тези полета.

От така въведената форма генерираме потребители със следната структура:

```scala
case class User(
  name: String,
  email: Email,
  passwordHash: String,
  birthday: Date,
  postalCode: Option[String]
)
```

Вашата задача е да имплементирате следната функция в обекта `UserRegistration`:

```scala
def registerUser(
  userCountryPostalCodeVerifier: String => Boolean,
  today: Date
)(form: RegistrationForm): Validated[RegistrationFormError, User] = ???
```

Тук `userCountryPostalCodeVerifier` е предикат, казващ ни дали определен пощенски код е валиден, а `today` е днешната дата. Функцията приема въведената форма, валидира я по всички налични условия, и при успешна валидация генерира обект от типа `User`. Условията, на които една валидно попълнена регистрационна форма отговаря, са следните:

* Полето за име е непразно
* Попълненият email е валиден. За валиден email считайте низ с точно един символ `@` и символи отляво и отдясно на него
* Попълнената парола е с поне 8 символа
* Попълнената парола съдържа поне една буква, цифра и специален символ (т.е. символ, който не е буква или цифра)
* Попълнената парола (`password`) и `passwordConfirmation` съвпадат
* `birthYear`, `birthMonth` и `birthDay` са цели числа
* `birthMonth` е между 1 и 12
* `birthDay` е между 1 и 31
* Получената дата от `birthYear`, `birthMonth` и `birthDay` е съществуваща и валидна. Използвайте методът `Date.applyOption` за да разберете това
* Получената рождена дата не е в бъдещето
* Ако пощенският код е въведен, тоест е непразен низ, то той трябва да е валиден според предиката `userCountryPostalCodeVerifier`

В `UserRegistration.scala` ще намерите типове грешки, съответстващи на всяка от тези ситуации. Очакваме от вас да върнете всички грешки, които са приложими, в произволен ред. 

Забележете, че това означава само грешки, които могат да бъдат проверени. Например, ако `birthMonth` не е число, то не очакваме грешката за несъществуваща дата, тъй като няма как да бъде проверена. Аналогично и ако месецът е извън интервала [1, 12]. Ако датата е невалидна, то също няма как да бъде проверено условието дали датата е в бъдещето или не. Ако обаче и `birthYear` и `birthMonth` не са числа, то очакваме да получим и двете грешки. Тези разсъждения ни подзсказват, че някои `Validated` инстанции ще комбинираме чрез `zip` (и подобните му функции), а други чрез `flatMap`.

Затова ще дефинираме следните зависимости:

* `NameIsEmpty`, `InvalidEmail`, `PasswordTooShort`, `PasswordRequiresGreaterSymbolVariety`, `PasswordsDoNotMatch`, `InvalidBirthdayDate` и `InvalidPostalCode` са независими и могат да бъдат проверени директно
* `BirthdayDateIsInTheFuture` може да се генерира само ако валидацията за рождена дата е минала успешно
* `InvalidBirthdayDate`съдържа `Chain` от всички грешки, свързани с датата за рожден ден. Идеята е валидацията на дата да се имплементира независимо от валидацията на регистрационна форма.
* При валидация на дата `YearIsNotAnInteger`, `MonthIsNotAnInteger` и `DayIsNotAnInteger` се независими; `MonthOutOfRange` и `DayOutOfRange` се проверяват само ако съответно месецът или денят са валидно въведени числа; а `InvalidDate` се проверява само ако всички останали валидации на датата са минали успешно.

От въведените във формата полета единствено `postalCode` е незадължително. Така ако не е попълнено получаваме валидна стойност от тип `None`.

За да генерирате `passwordHash` използвайте `PasswordUtils.hash`.

След като имплементирате функцията може да я тествате чрез кратката програма `UserRegistrationApp`.

## Стил (3 точки*)

Това, което също ще искаме да оценим в тази задача, е стилът, който ще приложете, както при имплементацията на `Chain` и `Validated`, но особено при валидацията на регистрационна форма, където имате пълна свобода сами да дизайнете кода.

За изключително добри решения ще получите и допълнително над тези 3 точки, Аналогично, при лош стил и ненужно използване на странични ефекти, `var` или `return`, си запазваме правото и да отнемем точки.

## Форматиране

Някои от вас имаха проблеми във форматирането на Scala кода в предното домашно. За да ви насочим към консистентен стил към това домашно сме добавили инструмента [`scalafmt`](https://scalameta.org/scalafmt/). За да форматирате кода си може да използвате `scalafmt` командата на `sbt` (идваща от `sbt-scalafmt` plugin-а, който сме добавили). Вижте [документацията на scalafmt](https://scalameta.org/scalafmt/docs/installation.html) за как да интегрирате и вашите IDE или редактор.

В `.scalafmt.conf` сме добавили наша конфигурация за формат. Чувствайте се напълно свободни да я промените, ако предпочитате различен стил. Важното е накрая кодът да е консистентен.

## Допълнителни указания и оценяване

Използвайте проекта, предоставен в тази директория. Насърчаваме ви да добавите собствени тестове, за да сте сигурни, че вашата имплементация е правилна.

Също, за да можем да изпълним и нашите тестове, моля предайте код, който се компилира, и при който не сте променили предоставения интерфейс. Ако не сте имплементирали някоя от функциите, моля все пак да не я изтривате, тъй като иначе ще видим компилационна грешка. 

Цялостното решения на задачата ви носи 10 точки.

## Напътствия

* Pattern matching-ът е изключително полезен при структурни трансформации.
* Свободно добавяйте каквито помощни функции сметнете за необходими. Особено при имплементацията на регистрационната форма. Не се колебайте също така да добавите метод извън изброените към `Validated` или `Chain` ако той би подобрил решението.
* Когато имплементирате нова операция на `Validated` или `Chain` не забравяйте да помислите дали тя не може да бъде изразена чрез някоя от другите.
* Добавили сме за вас малка програмка `UserRegistrationApp`, която използва вашето решение и разгледания на лекциите ефект `IO` за да демонстрира как можем да комбинираме всичко, за което си говорим, в едно цялостно приложение. Внимателно разгледайте кода. Забележе как единствено място с реални странични ефекти е едва последният ред на кода.
* Използвайте [Scala API Docs](https://www.scala-lang.org/api/current/) или функционалността на вашето IDE за изследване на типовете за да откриете повече за типовете и обектите от стандартната библиотека, с които работите – доста често те имат методи, подходящи точно за проблемът, който решавате.
* Не се притеснявайте да ни питате всякакви въпроси.
