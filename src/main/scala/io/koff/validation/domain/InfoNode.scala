//InfoNode.scala
package io.koff.validation.domain

/**
 * Generic recursive example of domain class
 * @param index here it is just a number without additional meaning
 * @param value some value which we want to store in a node and validate
 * @param children child nodes which should also be validated
 * @tparam T value type
 */
case class InfoNode[T](index: Int, value: Option[T], children: Seq[InfoNode[T]])

object InfoNode{
  /**
   * Just a default value for InfoNode
   */
  def default[T]: InfoNode[T] = InfoNode(1, None, Seq.empty)
}
