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

import java.io._
import java.nio.charset.Charset
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.errors.error.NotCoreFileError
import scala.util.{ Try => UTry, Failure => UFailure }

// CORE Programs
case class Program(insts: List[Inst]) {
  def appendTo(sb: StringBuilder, indent: String = ""): StringBuilder = {
    insts.foreach(_.appendTo(sb, indent).append(LINE_SEP))
    sb
  }
}

// parser for programs
trait ProgramParser extends InstParser {
  val prog: PackratParser[Program] = instSeq ^^ { Program(_) }
}
object Program extends ProgramParser {
  // parse a string into a program
  def apply(str: String): Program = parseAll(prog, str).get

  // parse a file into sequence of instructions
  def fileToInsts(f: String): UTry[List[Inst]] = {
    var fileName = new File(f).getCanonicalPath
    if (File.separatorChar == '\\') {
      // convert path string to linux style for windows
      fileName = fileName.charAt(0).toLower + fileName.replace('\\', '/').substring(1)
    }
    if (fileName.endsWith(".core")) {
      val fs = new FileInputStream(new File(f))
      val sr = new InputStreamReader(fs, Charset.forName("UTF-8"))
      val in = new BufferedReader(sr)
      val result = UTry(parseAll(instSeq, in).get)
      in.close; sr.close; fs.close
      result
    } else UFailure(NotCoreFileError(fileName))
  }

  // parse files into a program
  def filesToCore(fs: List[String]): UTry[Program] = fs match {
    case files => (files.foldLeft(UTry(List[Inst]())) {
      case (res, f) => fileToInsts(f).flatMap {
        case ss => res.flatMap { case l => UTry(l ++ ss) }
      }
    }).map { Program(_) }
  }
}
