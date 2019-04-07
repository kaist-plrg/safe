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

// CORE Left-Hand-Sides
sealed trait Lhs extends CoreNode {
  def getRef: Ref = this match {
    case LhsRef(ref) => ref
    case LhsLet(id) => RefId(id)
  }
}
case class LhsRef(ref: Ref) extends Lhs
case class LhsLet(id: Id) extends Lhs
