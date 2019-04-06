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

// CORE Environments
case class Env(
  locals: Addr,
  retCont: Option[Cont] = None,
  excCont: Option[Cont] = None
) extends CoreNode
