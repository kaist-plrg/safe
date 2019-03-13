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
import org.scalatest._
import scala.util.Random.shuffle

class EvalTest extends AnalyzeTest {
  // tests for interpreter
  def evalHelper(file: File): Unit = {
    val filename = file.getName
    lazy val name = file.toString
    lazy val config = safeConfig.copy(fileNames = List(name))
    if (jsFilter(filename)) {
      test(s"[Eval] $filename") {
        val safeConfig = config.copy(fileNames = List(name))
        CmdEval(List("-silent", name))
        ???
      }
    }
  }

  // registration
  val evalTestDir = testDir + "core"
  shuffle(walkTree(new File(evalTestDir))).foreach(evalHelper(_))
}
