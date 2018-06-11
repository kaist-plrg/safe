/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.cdomain

/**
 * Created by ysko on 2015. 11. 21..
 */
// a set of Value
sealed trait CValue

object CValue {
  def apply(i: Int): CValue = CNumberLong(i)
  def apply(i: Long): CValue = CNumberLong(i)
  def apply(s: String): CValue = if (s != null) CString(s) else CNull
  def apply(b: Boolean): CValue = CBoolean(b)
  def loc(s: String): CValue = if (s != null) CLoc(s) else CNull
  def unknownString: CValue = CStringUnknown
  def unknownNumber: CValue = CIntRange(Long.MinValue, Long.MaxValue)
}

// A singleton set of locations
case class CLoc(loc: String) extends CValue
// A singleton set of booleans
case class CBoolean(bool: Boolean) extends CValue
case class CNumberLong(number: Long) extends CValue
case class CNumberDouble(number: Double) extends CValue
case object CNumberPInf extends CValue
case object CNumberNInf extends CValue
case object CNumberNaN extends CValue
// A singleton set of strings
case class CString(str: String) extends CValue
case object CStringUnknown extends CValue
// A singleton set of undefined values
case object CUndefined extends CValue
// A singleton set of null values
case object CNull extends CValue
// A set of integers represented by an interval.
case class CIntRange(min: Long, max: Long) extends CValue
