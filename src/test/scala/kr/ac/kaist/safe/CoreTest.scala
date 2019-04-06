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
import kr.ac.kaist.safe.phase._
import kr.ac.kaist.safe.nodes.core._
import org.scalatest._
import scala.util.Random.shuffle
import scala.util.{ Failure, Success, Try }

class CoreTest extends SafeTest {
  // basic core files
  val coreDir = testDir + "core" + SEP

  // tests for core-parser
  def parseCoreTest(pgm: Try[Program]): Unit = pgm match {
    case Failure(e) => fail(s"it throws an error: $e")
    case Success(pgm) =>
      val newPgm = Parser.parseProgram(beautify(pgm))
      assert(pgm == newPgm)
  }

  // tests for core-interpreter
  def evalCoreTest(st: Try[State]): Unit = st match {
    case Failure(e) => fail(s"it throws an error: $e")
    case Success(st) =>
  }

  // basic core files
  val basicCoreDir = coreDir + "basic" + SEP

  // registration
  for (file <- shuffle(walkTree(new File(basicCoreDir)))) {
    val filename = file.getName
    if (coreFilter(filename)) {
      lazy val name = file.toString
      lazy val config = safeConfig.copy(fileNames = List(name))

      lazy val pgm = ParseCore((), config)
      test(s"[ParseCore] $filename") { parseCoreTest(pgm) }

      lazy val st = pgm.flatMap(EvalCore(_, config))
      test(s"[EvalCore] $filename") { evalCoreTest(st) }
    }
  }
}
