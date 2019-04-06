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

// CORE References
sealed trait Ref extends CoreNode
case class RefId(id: Id) extends Ref
case class RefIdProp(ref: Ref, id: Id) extends Ref
case class RefStrProp(ref: Ref, expr: Expr) extends Ref
