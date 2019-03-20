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
case class Id(name: String) {
  def appendTo(sb: StringBuilder): StringBuilder = sb.append(name)
}

// parser for identifiers
trait IdParser extends CoreParser {
  val id: PackratParser[Id] = ident ^^ { Id(_) }
}
