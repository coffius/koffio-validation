package io.koff.validation

import io.koff.validation.domain._

/**
 * Example of using Validation
 * https://github.com/davegurnell/validation
 */
object UnderscoreValidation {
  import io.underscore.validation._

  /**
   * Recursive validator for InfoNode
   */
  def infoNode[T](rule: Validator[T]): Validator[InfoNode[T]] = Validator[InfoNode[T]] { in =>
    val generalValidator = validate[InfoNode[T]]
      .field(_.index)(gte(1, "InfoNode.index.gte"))
      .field(_.children)(lengthLte(2, "InfoNode.children.lengthLte"))
      .field(_.value)(optional(rule))

    val resultValidator = if(in.children.isEmpty) {
      generalValidator
    } else {
      generalValidator.seqField(_.children)(infoNode(rule))
    }

    resultValidator(in)
  }

  /**
   * Define custom validators for io.underscore.validation
   */
  def startWith(value: => String, msg: => String): Validator[String] = Validator[String] { in =>
    if(in.startsWith(value)) pass else fail(msg)
  }

  def endWith(value: => String, msg: => String): Validator[String] = Validator[String] { in =>
    if(in.endsWith(value)) pass else fail(msg)
  }

//  implicit val infoNodeValidator = validate[InfoNode]
//    .field(_.index)(gte(1, "InfoNode.index.gte"))

  implicit val phoneValidator = validate[Phone].field(_.value)(startWith("+7", "Phone.value.startWith"))

  implicit val addressValidator = validate[Address]
    .field(_.street)(nonEmpty("Address.street.notEmpty"))
    .field(_.house)(gt(0, "Address.house.gt"))

  implicit val userValidator = validate[User]
    .field(_.login)(nonEmpty  ("User.login.notEmpty"))
    .field(_.login)(startWith ("super_", "User.login.startWith") and endWith("!", "User.login.endWith"))
    .field(_.email)(matchesRegex("""\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\z""".r, "User.login.invalid"))

    .field(_.password)(lengthGt   (6, "User.password.lengthGt"))
    .field(_.password)(lengthLte  (12, "User.password.lengthLte"))

    .field(_.age)(gte(13, "User.age.gte"))
    .field(_.age)(lte(99, "User.age.lte"))
    .field(_.age)(eql(14, "User.age.eql"))
    .field(_.phone)(optional(phoneValidator))
  // also you can use `required(...)` if you want to have a defined optional field
  //.field(_.phone)(required(phoneValidator))
  //and `.seqField(...)` for validate Seq[T]
    .field(_.addresses)(nonEmpty("User.address.notEmpty"))
    .field(_.userInfo)(infoNode(nonEmpty("User.userInfo.value.notEmpty")))              //using recursion
    .field(_.userInfo)(infoNode(startWith("correct_", "User.userInfo.value.startWith")))//using recursion
    .seqField(_.addresses)

  def main(args: Array[String]) {
    val correctUser = User(
      login = "super_user_!",
      email = "example@example.com",
      password = "1234567",
      age = 14,
      phone = Some(Phone("+78889993322")),
      addresses = Seq(Address("Baker st.", 221)),
      userInfo = InfoNode(
        index = 1,
        value = Some("correct_parent"),
        children = Seq(
          InfoNode(10, Some("correct_child"), Seq.empty),
          InfoNode(20, None, Seq.empty)
        )
      )
    )

    println("correctUser: " + correctUser.validate.errors)
    // prints: 'correctUser: List()'

    val allWrong = correctUser.copy(
      login = "not_super_user",
      email = """"Look at all these spaces!"@example.com""", //it is still a valid email address
      password = "short",
      age = 101,
      addresses = Seq(Address("", -1))
    )

    println("allWrong: " + allWrong.validate)
    //prints: 'allWrong:
    // Validated(
    //    User(
    //      not_super_user,
    //      "Look at all these spaces!"@example.com,
    //      short,
    //      101,
    //      Some(Phone(+78889993322)),
    //      List(Address(,-1)),
    //      InfoNode(1,Some(correct_parent),List(InfoNode(10,Some(correct_child),List()), InfoNode(20,None,List())))
    //    ),
    //    List(
    //      ValidationError(User.login.startWith,ValidationPath(login)),
    //      ValidationError(User.login.endWith,ValidationPath(login)),
    //      ValidationError(User.login.invalid,ValidationPath(email)),
    //      ValidationError(User.password.lengthGt,ValidationPath(password)),
    //      ValidationError(User.age.lte,ValidationPath(age)),
    //      ValidationError(User.age.eql,ValidationPath(age)),
    //      ValidationError(Address.street.notEmpty,ValidationPath(addresses[0].street)),
    //      ValidationError(Address.house.gt,ValidationPath(addresses[0].house))
    //    )
    // )
    //'

    val withoutAddresses = correctUser.copy(addresses = Seq.empty)
    println("withoutAddresses: " + withoutAddresses.validate.errors)

    val withWrongAddress = correctUser.copy(
      addresses = Seq(
        Address("Correct Street", 1),
        Address("", -1)
      )
    )
    println("withWrongAddress: " + withWrongAddress.validate.errors)

    val withWrongNodes = correctUser.copy(
      userInfo = InfoNode(
        index = 1,
        value = Some("incorrect_parent_value"),
        children = Seq(
          InfoNode(10, Some("correct_child"), Seq.empty),
          InfoNode(20, None, Seq.empty),
          InfoNode(30, Some("superfluous_child"), Seq.empty)
        )
      )
    )
    println("withWrongNodes: " + withWrongNodes.validate.errors)

    /**
     * davegurnell/validation is a great lib for validation of very complex structures like generics and recursions
     * if you are not bothered concerning dependence on `scala.language.experimental.macros` and `scala.language.higherKinds`
     * which are used in this lib.
     */
  }
}
