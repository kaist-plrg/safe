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

import kr.ac.kaist.safe.LINE_SEP

// CORE Continuations
trait Cont
case class LabelCont(insts: List[Inst]) extends Cont
case class ScopeCont(id: Id, insts: List[Inst], env: Env) extends Cont
