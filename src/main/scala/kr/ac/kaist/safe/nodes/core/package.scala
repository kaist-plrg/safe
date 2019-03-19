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
import scala.util.parsing.combinator.{ JavaTokenParsers, PackratParsers }

package object core {
  def error(msg: String): Nothing = throw CoreError(msg)
  def cast[T](v: Any)(implicit tag: ClassTag[T]): T =
    v match { case v: T => v case _ => error(s"$v is not $tag") }

  // indentation
  val TAB = "  "

  // parsers
  trait CoreParser extends JavaTokenParsers with PackratParsers {
    val idstr: PackratParser[String] = ident
  }
}
