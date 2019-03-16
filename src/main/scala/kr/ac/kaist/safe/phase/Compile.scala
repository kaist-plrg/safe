/**
 * *****************************************************************************
 * Copyright (c) 2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by thcored parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase

import scala.util.{ Try, Success }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.compiler.Compiler
import kr.ac.kaist.safe.nodes.ast
import kr.ac.kaist.safe.nodes.core
import kr.ac.kaist.safe.util._

// Compile phase
case object Compile extends PhaseObj[ast.Program, CompileConfig, core.Program] {
  val name: String = "compiler"
  val help: String = "Compiles JavaScript source files to Core."

  def apply(
    program: ast.Program,
    safeConfig: SafeConfig,
    config: CompileConfig
  ): Try[core.Program] = {
    // Compile AST -> Core.
    val compiler = new Compiler(program)
    val core = compiler.result

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(core.toString)
        writer.close; fw.close
        println("Dumped core to " + out)
      }
      case None =>
    }
    Success(core)
  }

  def defaultConfig: CompileConfig = CompileConfig()
  val options: List[PhaseOption[CompileConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during compilation are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the resulting Core will be written to the outfile.")
  )
}

// Compile phase config
case class CompileConfig(
  var silent: Boolean = false,
  var outFile: Option[String] = None
) extends Config
