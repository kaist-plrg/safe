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

// CORE Properties
sealed trait Prop extends CoreNode
case class GlobalId(id: Id) extends Prop
case class PropId(addr: Addr, id: Id) extends Prop
case class PropStr(addr: Addr, str: String) extends Prop
