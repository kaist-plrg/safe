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

// CORE Labels
abstract class Label {
  def appendTo(sb: StringBuilder): StringBuilder = this match {
    case UserLabel(name) => sb.append(name)
    case ReturnLabel => sb.append("@return")
    case ThrowLabel => sb.append("@throw")
  }
}
case class UserLabel(name: String) extends Label
case object ReturnLabel extends Label
case object ThrowLabel extends Label

// parser for expressions
trait LabelParser extends CoreParser {
  val label: PackratParser[Label] =
    "@return" ^^^ ReturnLabel |
      "@throw" ^^^ ThrowLabel |
      idstr ^^ { UserLabel(_) }
}
