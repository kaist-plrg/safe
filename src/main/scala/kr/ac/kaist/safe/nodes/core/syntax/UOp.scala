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

// CORE Unary Operators
abstract class UOp(val keyword: String) extends Op
case object ONeg extends UOp("-")
case object OBNot extends UOp("!")

// parser for unary operators
trait UOpParser extends CoreParser {
  lazy val uop = "-" ^^^ ONeg | "!" ^^^ OBNot
}
