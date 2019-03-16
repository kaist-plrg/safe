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

// CORE Programs
case class Program(insts: List[Inst]) {
  def appendTo(sb: StringBuilder, indent: String = ""): StringBuilder = {
    insts.foreach(_.appendTo(sb, indent).append(LINE_SEP))
    sb
  }
}

// parser for programs
trait ProgramParser extends InstParser {
  lazy val prog = instSeq
}
