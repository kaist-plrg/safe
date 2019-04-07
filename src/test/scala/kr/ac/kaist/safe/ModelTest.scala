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

class CoreModelTest extends SafeTest {
  // tests for core-parser
  def parseCoreTest(func: Func): Unit = {
    val newFunc = Parser.parseFunc(beautify(func))
    assert(func == newFunc)
  }

  // basic core files
  val coreModelDir = resourceDir + "coreModels" + SEP

  // registration
  for (file <- shuffle(walkTree(new File(coreModelDir)))) {
    val filename = file.getName
    if (funcFilter(filename)) {
      lazy val name = file.toString
      lazy val func = Parser.fileToFunc(name)
      test(s"[ParseCoreModel] $filename") { parseCoreTest(func) }
    }
  }
}
