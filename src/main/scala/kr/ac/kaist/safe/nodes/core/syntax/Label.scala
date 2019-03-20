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
case class Label(name: String) {
  def appendTo(sb: StringBuilder): StringBuilder = sb.append(name)
}

// parser for expressions
trait LabelParser extends CoreParser {
  val label: PackratParser[Label] = ident ^^ { Label(_) }
}
