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

import kr.ac.kaist.safe.LINE_SEP
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
    override protected val whiteSpace = """(\s|//.*)+""".r
  }

  // types
  type Ty = String

  // identifiers
  type Id = String

  // labels
  type Label = String

  ////////////////////////////////////////////////////////////////////////////////
  // appendTo
  ////////////////////////////////////////////////////////////////////////////////
  // type
  trait Appendable {
    def appendTo(
      sb: StringBuilder,
      indent: String,
      firstIndent: Boolean,
      detail: Boolean
    ): StringBuilder
  }

  // for map structure
  def appendMap[K, V <: Appendable](
    sb: StringBuilder,
    map: Map[K, V],
    indent: String,
    detail: Boolean
  ): StringBuilder = (sb /: map) {
    case (sb, (k, v)) =>
      sb.append(indent).append(k).append(" -> ")
      v.appendTo(sb, indent, false, detail).append(LINE_SEP)
  }

  // for option structure
  def appendOpt[T <: Appendable](
    sb: StringBuilder,
    opt: Option[T],
    indent: String,
    detail: Boolean
  ): StringBuilder = opt match {
    case Some(content) =>
      sb.append(LINE_SEP)
      content.appendTo(sb, indent, false, detail)
    case None => sb.append("empty").append(LINE_SEP)
  }
}
