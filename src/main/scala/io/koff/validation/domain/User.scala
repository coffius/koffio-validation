//User.scala
package io.koff.validation.domain

/**
 * User - the main example domain class for validation
 * @param login user login
 * @param email user email
 * @param password user password
 * @param age user age
 * @param phone user phone
 * @param addresses list of user addresses - for showing a sequence validation
 * @param userInfo recursive data for showing a recursive validation
 */
case class User(login: String,
                email: String,
                password: String,
                age: Int,
                phone: Option[Phone],
                addresses: Seq[Address],
                userInfo: InfoNode[String] = InfoNode.default)
