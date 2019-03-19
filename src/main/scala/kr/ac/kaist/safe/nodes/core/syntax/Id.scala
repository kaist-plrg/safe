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
    case InternalId(name) => sb.append("@").append(name)
  }
}

// user identifiers
case class UserId(name: String) extends Id

// internal identifiers
case class InternalId(name: String) extends Id

// parser for identifiers
trait IdParser extends CoreParser {
  val id: PackratParser[Id] = idstr ^^ { UserId(_) } | "@" ~> idstr ^^ { InternalId(_) }
}
object Id extends IdParser {
  def apply(str: String): Id = parseAll(id, str).get
}
