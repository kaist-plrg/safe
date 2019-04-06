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
sealed trait UOp extends CoreNode
case object ONeg extends UOp
case object ONot extends UOp
case object OBNot extends UOp
