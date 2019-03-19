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

// CORE Operators
abstract class Op {
  val keyword: String
  override def toString: String = keyword
}

// parser for operators
trait OpParser extends UOpParser with BOpParser {
  val op: PackratParser[Op] = uop | bop
}
object Op extends OpParser {
  def apply(str: String): Op = parseAll(op, str).get
}
