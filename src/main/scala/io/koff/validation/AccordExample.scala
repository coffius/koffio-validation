package io.koff.validation

/**
 * Example of using Accord
 * https://github.com/wix/accord
 * http://wix.github.io/accord/
 */
object AccordExample {
  import io.koff.validation.domain.{Address, User, Phone}
  import com.wix.accord._
  import dsl._

  /**
   * Define Accord validator for Address class
   */
  implicit val addressValidator = validator[Address] { address =>
    address.street is notEmpty
    address.house is >(0)
  }

  /**
   * Define Accord validator for Phone class
   */
  implicit val phoneValidator = validator[Phone]{ phone =>
    phone.value is notEmpty and startWith("+7")
  }

  /**
   * Define Accord validator for User class
   */
  implicit val userValidator = validator[User] { user =>
    //It is not possible to make a negative predicate
    //for example there is no way to forbid using "admin" in user name except for creation of your own validator
    user.login is notEmpty and startWith("super_") and endWith("!")

    //just as a sample :)
    //don`t user regex to validate emails:
    // - http://davidcel.is/posts/stop-validating-email-addresses-with-regex/
    //otherwise your users can face with problems using their emails on your service
    user.email is matchRegex("""\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\z""")

    // all the definitions below are equal
    user.password.length is between(6, 12)
    user.password have size >= 6
    user.password has size <= 12

    //you can validate number with range
    user.age.must(between(13, 99))

    //can use >, <, >=, <= etc
    user.age must <=(99)
    user.age must >=(13)
    //can`t write it like this
    //user.age must >= 13
    //it will be a compilation error

    //check if a number field is equal or not equal to some value
    //user.age must equalTo(1000)
    //user.age must notEqualTo(1)

    //checks if an option field is defined with Some(value)
    //user.phone is notEmpty

    //make sure that if the phone field is defined then it is defined with correct value
    user.phone.each is valid

    user.addresses should notEmpty
    user.addresses.each is valid
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

    println("correct result: " + validate(correctUser))
    //prints: 'correct result: Success'

    val withWrongPhone = correctUser.copy(phone = Some(Phone("8889993322")))
    println("withWrongPhone: " + validate(withWrongPhone))
    //prints: 'withWrongPhone: Failure(Set(GroupViolation(Phone(8889993322),is invalid,Some(phone),Set(RuleViolation(8889993322,must start with '+7',Some(value))))))'

    //it will work if `user.phone is notEmpty` is commented
    val withoutPhone = correctUser.copy(phone = None)
    println("withoutPhone: " + validate(withoutPhone))

    val allWrong = correctUser.copy(
      login = "not_super_user",
      email = """"Look at all these spaces!"@example.com""", //it is still a valid email
      password = "short",
      age = 101
    )

    println("withoutPhone: " + validate(allWrong))

    val withoutAddresses = correctUser.copy(addresses = Seq.empty)
    println("withoutAddresses: " + validate(withoutAddresses))

    val withWrongAddress = correctUser.copy(
      addresses = Seq(
        Address("Correct Street", 1),
        Address("", -1)
      )
    )
    println("withWrongAddress: " + validate(withWrongAddress))

    /**
     * In general this library is quite good but there is one issue:
     * an error message is represented as a sentence with a lexical description instead of an error code like "User.login.empty".
     * It can be a problem when you want to localize error messages
     */
  }
}
