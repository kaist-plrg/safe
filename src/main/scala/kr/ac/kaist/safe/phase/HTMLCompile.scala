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

import scala.util.{ Try, Success }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.compiler.Translator
import kr.ac.kaist.safe.nodes.ast.Program
import kr.ac.kaist.safe.nodes.ir.IRRoot
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.html.HTMLModel

// Compile phase
case object HTMLCompile extends PhaseObj[(HTMLModel.T, Program), CompileConfig, (HTMLModel.T, IRRoot)] {
  val name: String = "HTMLCompiler"
  val help: String = "Translates JavaScript source files to IR."

  def apply(
    in: (HTMLModel.T, Program),
    safeConfig: SafeConfig,
    config: CompileConfig
  ): Try[(HTMLModel.T, IRRoot)] = {
    val (html, program) = in
    // Translate AST -> IR.
    val translator = new Translator(program, config.keyVariables)
    val ir = translator.result
    val excLog = translator.excLog

    // Report errors.
    if (excLog.hasError) {
      println(ir.relFileName + ":")
      println(excLog)
    }

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(ir.toString(0))
        writer.close; fw.close
        println("Dumped IR to " + out)
      }
      case None =>
    }
    Success((html, ir))
  }

  def defaultConfig: CompileConfig = CompileConfig()
  val options: List[PhaseOption[CompileConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during compilation are muted."),
    ("keyvars", BoolOption(c => c.keyVariables = true),
      "it performs a pre-analysis to find key variables in loops."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the resulting IR will be written to the outfile.")
  )
}
