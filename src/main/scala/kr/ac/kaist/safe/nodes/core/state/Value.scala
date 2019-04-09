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

// CORE Values
sealed trait Value extends CoreNode

// CORE Addresses
sealed trait Addr extends Value
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

// CORE Functions
case class Func(params: List[Id], body: Inst) extends Value

// CORE Constants
sealed trait Const extends Value
case class Num(double: Double) extends Const {
  override def equals(that: Any): Boolean = that match {
    case that: Num => doubleEquals(this.double, that.double)
    case _ => false
  }
}
case class INum(long: Long) extends Const
case class Str(str: String) extends Const
case class Bool(bool: Boolean) extends Const
case object Undef extends Const
case object Null extends Const
