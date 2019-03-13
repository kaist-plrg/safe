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

class CFGBuildTest extends SafeTest {
  // javascript files
  val jsDir = testDir + "cfg" + SEP + "js" + SEP + "success" + SEP

  // result files
  val resDir = testDir + "cfg" + SEP + "result" + SEP + "success" + SEP

  // test directory
  def resDir(phase: String): String = s"$resDir$phase$SEP"
  lazy val jsToTest = changeExt("js", "test")
  lazy val astRewriteResDir = resDir("astRewrite")
  lazy val translateResDir = resDir("compile")
  lazy val cfgBuildResDir = resDir("cfg")

  // registration
  for (file <- shuffle(walkTree(new File(jsDir)))) {
    val filename = file.getName
    if (jsFilter(filename)) {
      lazy val name = file.toString
      lazy val resName = jsToTest(filename)
      lazy val config = safeConfig.copy(fileNames = List(name))

      lazy val pgm = Parse((), config)
      test(s"[Parse] $filename") { parseTest(pgm) }

      lazy val ast = pgm.flatMap(ASTRewrite(_, config))
      lazy val astResName = astRewriteResDir + resName
      test(s"[ASTRewrite] $filename") { astRewriteTest(ast, astResName) }

      lazy val ir = ast.flatMap(Translate(_, config))
      lazy val translateResName = translateResDir + resName
      test(s"[Translate] $filename") { translateTest(ir, translateResName) }

      lazy val cfg = ir.flatMap(CFGBuild(_, config))
      lazy val cfgBuildResName = cfgBuildResDir + resName
      test(s"[CFGBuild] $filename") { cfgBuildTest(cfg, cfgBuildResName) }
    }
  }
}
