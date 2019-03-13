/**
 * *****************************************************************************
 * Copyright (c) 2016-2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe

import java.io._
import kr.ac.kaist.safe.nodes.ast.Program
import kr.ac.kaist.safe.nodes.cfg.CFG
import kr.ac.kaist.safe.nodes.ir.IRRoot
import kr.ac.kaist.safe.parser.Parser
import kr.ac.kaist.safe.phase._
import kr.ac.kaist.safe.util._
import org.scalatest._
import scala.io.Source
import scala.util.{ Failure, Success, Try }

abstract class SafeTest extends FunSuite {
  // tests directory
  lazy val testDir = BASE_DIR + SEP + "tests" + SEP

  // safe configuration
  lazy val safeConfig: SafeConfig = SafeConfig(CmdBase, Nil)

  ////////////////////////////////////////////////////////////////////////////////
  // Helper Functions
  ////////////////////////////////////////////////////////////////////////////////
  // filename filters
  def extFilter(ext: String): String => Boolean = _.endsWith(s".$ext")
  lazy val jsFilter = extFilter("js")
  lazy val htmlFilter = extFilter("html")
  lazy val errFilter = extFilter("err")

  // normalization
  def norm(s: String): String = s.replaceAll("\\s+", "").replaceAll("\\n+", "")

  // read a file
  def readFile(filename: String): String = {
    assert(new File(filename).exists)
    norm(Source.fromFile(filename).getLines.mkString(LINE_SEP))
  }

  // walk file tree
  def walkTree(file: File): Iterable[File] = {
    val children = new Iterable[File] {
      def iterator: Iterator[File] = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
    }
    Seq(file) ++: children.flatMap(walkTree(_))
  }

  // change extension
  def changeExt(from: String, to: String): String => String =
    filename => filename.substring(0, filename.length - from.length) + to

  // tests for parser
  def parseTest(pgm: Try[Program]): Unit = pgm match {
    case Failure(e) => fail(s"it throws an error: $e")
    case Success(program) =>
      Parser.stringToAST(program.toString(0)) match {
        case Failure(e) => fail(s"it throws an error: $e")
        case Success((pgm, _)) =>
          val pretty = pgm.toString(0)
          Parser.stringToAST(pretty) match {
            case Failure(e) => fail(s"it throws an error: $e")
            case Success((p, _)) =>
              assert(norm(p.toString(0)) == norm(pretty))
          }
      }
  }

  // tests for AST rewriter
  def astRewriteTest(ast: Try[Program], testName: String): Unit = {
    ast match {
      case Failure(_) => assert(false)
      case Success(program) =>
        assert(readFile(testName) == norm(program.toString(0)))
    }
  }

  // tests for translator
  def translateTest(ir: Try[IRRoot], testName: String): Unit = {
    ir match {
      case Failure(_) => assert(false)
      case Success(ir) =>
        assert(readFile(testName) == norm(ir.toString(0)))
    }
  }

  // tests for CFG builder
  def cfgBuildTest(cfg: Try[CFG], testName: String): Unit = {
    cfg match {
      case Failure(_) => assert(false)
      case Success(cfg) =>
        assert(readFile(testName) == norm(cfg.toString(0)))
    }
  }
}
