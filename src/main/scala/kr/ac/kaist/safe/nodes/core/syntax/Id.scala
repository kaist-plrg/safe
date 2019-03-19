/**
 * *****************************************************************************
 * Copyright (c) 2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.nodes.core

// CORE Identifiers
abstract class Id {
  def appendTo(sb: StringBuilder): StringBuilder = this match {
    case UserId(name) => sb.append(name)
    case GlobalId => sb.append("@global")
    case ReturnId => sb.append("@return")
    case ExceptionId => sb.append("@exception")
  }
}
case class UserId(name: String) extends Id
case object GlobalId extends Id
case object ReturnId extends Id
case object ExceptionId extends Id

// parser for expressions
trait IdParser extends CoreParser {
  val id: PackratParser[Id] =
    idstr ^^ { UserId(_) } |
      "@global" ^^^ GlobalId |
      "@return" ^^^ ReturnId |
      "@exception" ^^^ ExceptionId
}
