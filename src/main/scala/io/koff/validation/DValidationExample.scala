package io.koff.validation

import io.koff.validation.domain.{Address, User, Phone}
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.{Validator, _}

/**
 * Example of using DValidation
 * https://github.com/tobnee/DValidation/
 */
object DValidationExample {
  // We need to import scalaz.Order[Int] for `isInRange` and other number checkers
  import scalaz.std.AllInstances._
  /**
   * Example of a custom validator for DValidation
   * @param toCheck value that should be checked
   * @param startWith value that `toCheck` should start with
   */
  def startWith(toCheck: String, startWith: String): DValidation[String] =
    ensure(toCheck)("error.dvalidation.start_with")(a => a.startsWith(startWith))

  /**
   * Simple regex checker
   */
  def regex(toCheck: String, regexStr: String): DValidation[String] = {
    val regex = regexStr.r
    ensure(toCheck)("error.dvalidation.regex")(a => regex.pattern.matcher(a).find())
  }

  val phoneValidator = Validator.template[Phone] { phone =>
    phone.validateWith(
      startWith(phone.value, "+7")
    )
  }

  val addressValidator = Validator.template[Address] { address =>
    address.validateWith(
      notBlank(address.street) forAttribute 'street,
      address.house.is_>(0) forAttribute 'house
    )
  }

  val userValidator = Validator.template[User]{ user =>
    user.validateWith(
      notBlank(user.login) forAttribute 'login,
      startWith(user.login, "super_") forAttribute 'login,
      ensure(user.login)("error.dvalidation.end_with")(_.endsWith("!")) forAttribute 'login,
      regex(user.email, """\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\z""") forAttribute 'email,

      //these definitions are equal
      isInRange(user.password.length, 6, 12) forAttribute Symbol("User.password.length.between"),
      user.password.length.is_>=(6) forAttribute Symbol("User.password.length.lessThan"),
      user.password.length.is_<=(12) forAttribute Symbol("User.password.length.greaterThan"),

      user.age.is_>=(13) forAttribute 'age,
      user.age.is_<=(99) forAttribute 'age,
      user.age.is_==(14) forAttribute 'age, // or you can use `is_===` for a strict check
      validOpt(user.phone)(phoneValidator),

      hasElements(user.addresses) forAttribute 'addresses
    ).withValidations(
      // You also can user validSequence(...) to validate elements of a collection
      validSequence(user.addresses, addressValidator)
    )
  }

  def main(args: Array[String]) {
    val correctUser = User(
      login = "super_user_!",
      email = "example@example.com",
      password = "1234567",
      age = 14,
      //it will also work if phone = None
      phone = Some(Phone("+78889993322")),
      addresses = Seq(Address("Baker st.", 221))
    )

    println("correct result: " + userValidator(correctUser))
    //prints: 'correct result:
    // Success(
    //   User(
    //     super_user_!,
    //     example@example.com,
    //     1234567,
    //     14,
    //     Some(Phone(+78889993322)),
    //     List(Address(Baker st.,221)),
    //     InfoNode(1,None,List())
    //   )
    // )
    //'

    val allWrong = correctUser.copy(
      login = "not_super_user",
      email = """"Look at all these spaces!"@example.com""", //it is still a valid email address
      password = "short",
      age = 101,
      phone = Some(Phone("invalid_phone"))
    )

    println("allWrong: " + userValidator(allWrong))
    //prints: 'allWrong:
    // Failure(
    //   DomainError(path: /login, value: not_super_user, msgKey: error.dvalidation.start_with),
    //   DomainError(path: /login, value: not_super_user, msgKey: error.dvalidation.end_with),
    //   DomainError(path: /email, value: "Look at all these spaces!"@example.com, msgKey: error.dvalidation.regex),
    //   DomainError(path: /User.password.length.between, value: 5, msgKey: error.dvalidation.notGreaterThen, args: 6,false),
    //   DomainError(path: /User.password.length.lessThan, value: 5, msgKey: error.dvalidation.notGreaterThen, args: 6,true),
    //   DomainError(path: /age, value: 101, msgKey: error.dvalidation.notSmallerThen, args: 99,true),
    //   DomainError(path: /age, value: 14, msgKey: error.dvalidation.notEqual, args: 101),
    //   DomainError(path: /, value: invalid_phone, msgKey: error.dvalidation.start_with)
    // )

    val withoutAddresses = correctUser.copy(addresses = Seq.empty)
    println("withoutAddresses: " + userValidator(withoutAddresses))

    val withWrongAddress = correctUser.copy(
      addresses = Seq(
        Address("Correct Street", 1),
        Address("", -1)
      )
    )
    println("withWrongAddress: " + userValidator(withWrongAddress))
  }

  /**
   * Although there are not many build-in validators in DValidation,
   * it can be considered as a good option for validation of complex structures
   * if it is ok to have dependency on `scalaz` in your project
   */
}
