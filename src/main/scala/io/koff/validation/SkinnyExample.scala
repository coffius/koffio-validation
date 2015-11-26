package io.koff.validation

/**
 * Example of using Skinny validator
 * http://skinny-framework.org/documentation/validator.html
 */
object SkinnyExample {
  import io.koff.validation.domain.{Address, User, Phone}
  import skinny.validator._

  implicit val userValidator = new EntityValidator[User] {
    override def getValidator(entity: User): Validator = {
      import skinny.validator.{email => correctEmail}
      import entity._
      Validator(
        param("login" -> login) is notEmpty & startWith("super_") & endWith("!"),
        // skinny.validator.email using regex for checking email
        param("email" -> email) is correctEmail,

        //these definitions are equal
        param("password" -> password) is minMaxLength(6, 12),
        param("password" -> password) is minLength(6),
        param("password" -> password) is maxLength(12),

        //these definitions are equal
        param("age" -> age) is intMinMaxValue(13, 99),
        param("age" -> age) is intMinValue(13), // >=
        param("age" -> age) is intMaxValue(99), // <=
        //this is how to check equation of values
        param("age" -> (age, 14)) is same

        //there is no build-in way to check options(Some|None) and elements of collections
      )
    }
  }

  /**
   * For `startWith` we should create custom ValidationRule
   */
  case class startWith(value: String) extends ValidationRule {
    def name = "startWith"
    def isValid(v: Any) = Option(v) match {
      case Some(x) => x.toString.startsWith(value)
      case None => true
    }
  }

  /**
   * For `endWith` we should create custom ValidationRule
   */
  case class endWith(value: String) extends ValidationRule {
    def name = "endWith"
    def isValid(v: Any) = Option(v) match {
      case Some(x) => x.toString.endsWith(value)
      case None => true
    }
  }

  def validate[T](entity: T)(implicit entityValidator: EntityValidator[T]): Either[Errors, _] = {
    val validator = entityValidator.getValidator(entity)
    val isOk = validator.validate()
    if(isOk){
      Right(())
    } else {
      Left(validator.errors)
    }
  }

  def main(args: Array[String]) {
    val correctUser = User(
      login = "super_user_!",
      email = "example@example.com",
      password = "1234567",
      age = 14,
      phone = Some(Phone("+78889993322")),
      addresses = Seq(Address("Baker st.", 221))
    )

    println("correctUser: " + validate(correctUser))
    //prints: 'correctUser: Right(())'

    val allWrong = correctUser.copy(
      login = "not_super_user",
      email = """"Look at all these spaces!"@example.com""", //it is still a valid email address
      password = "short",
      age = 101
    )

    println("correctUser: " + validate(allWrong))
    //prints:
    //Left(
    //  Errors(
    //    Map(
    //      age -> List(
    //        Error(name = intMinMaxValue, messageParams = List(13, 99)),
    //        Error(name = intMaxValue, messageParams = List(99)),
    //        Error(name = same, messageParams = List())
    //      ),
    //      password -> List(
    //        Error(name = minMaxLength, messageParams = List(6, 12)),
    //        Error(name = minLength, messageParams = List(6))
    //      ),
    //      email -> List(Error(name = email, messageParams = List())),
    //      login -> List(Error(name = startWith, messageParams = List()))
    //    )
    //  )
    //)'

    /**
     * Skinny validator has a basic functionality for validation of simple objects.
     * But if you want to validate more complex structures you have to write your own validation rules
     */
  }

  trait EntityValidator[T] {
    def getValidator(entity: T): Validator
  }
}
