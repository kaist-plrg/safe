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

package kr.ac.kaist.safe.nodes

package object core {
  // throw an error
  def error(msg: => String): Nothing = throw new Error(msg)

  // beautify
  def beautify(
    node: CoreNode,
    tab: String = "  ",
    detail: Boolean = true
  ): String = Beautifier.beautify(node, tab, detail)

  // equality between doubles
  def doubleEquals(left: Double, right: Double): Boolean = {
    if (left.isNaN && right.isNaN) true
    else if (isNegZero(left) && !isNegZero(right)) false
    else if (!isNegZero(left) && isNegZero(right)) false
    else left == right
  }

  // negative zero check
  def isNegZero(double: Double): Boolean = (1 / double).isNegInfinity
}
