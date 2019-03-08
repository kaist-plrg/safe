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

import kr.ac.kaist.safe.errors.error.CoreError
import scala.reflect.ClassTag

package object core {
  type Id = String
  type Label = String
  type Prop = String

  def error(msg: String): Nothing = throw CoreError(msg)
  def cast[T](v: Any)(implicit tag: ClassTag[T]): T =
    v match { case v: T => v case _ => error(s"$v is not $tag") }
}
