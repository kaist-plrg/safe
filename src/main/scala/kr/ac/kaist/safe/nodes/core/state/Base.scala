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

// CORE Bases
sealed trait Base extends CoreNode
case class BaseId(id: Id) extends Base
case class BaseIdProp(addr: Addr, id: Id) extends Base
case class BaseStrProp(addr: Addr, str: String) extends Base
