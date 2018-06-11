/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase

import scala.util.{ Success, Try }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.ast_rewriter.{ Disambiguator, Hoister, WithRewriter }
import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.nodes.ast.Program
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.html.HTMLModel

// ASTRewrite phase
case object HTMLASTRewrite extends PhaseObj[(HTMLModel.T, Program), ASTRewriteConfig, (HTMLModel.T, Program)] {
  val name: String = "HTMLASTRewriter"
  val help: String =
    "Rewrites AST in JavaScript source files (hoister, disambiguator, withRewriter)"

  def apply(
    in: (HTMLModel.T, Program),
    safeConfig: SafeConfig,
    config: ASTRewriteConfig
  ): Try[(HTMLModel.T, Program)] = {
    val (html, pgm) = in
    val (program, excLog) = rewrite(pgm)

    // Report errors.
    if (excLog.hasError && !safeConfig.testMode && !safeConfig.silent) {
      println(program.relFileName + ":")
      println(excLog)
    }

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(program.toString(0))
        writer.close; fw.close
        println("Dumped rewritten AST to " + out)
      }
      case None => return Try(html, program)
    }

    Success(html, program)
  }

  def rewrite(pgm: Program): (Program, ExcLog) = {
    // hoist
    val hoister = new Hoister(pgm)
    var program = hoister.result
    var excLog = hoister.excLog

    // disambiguate
    val disambiguator = new Disambiguator(program)
    program = disambiguator.result
    excLog += disambiguator.excLog

    // "with" rewrite
    val withRewriter = new WithRewriter(program, false)
    program = withRewriter.result
    excLog += withRewriter.excLog

    (program, excLog)
  }

  def defaultConfig: ASTRewriteConfig = ASTRewriteConfig()
  val options: List[PhaseOption[ASTRewriteConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during rewriting AST are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the rewritten AST will be written to the outfile.")
  )
}
