/**
 * *****************************************************************************
 * Copyright (c) 2016, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase

import java.io.{ BufferedWriter, FileWriter, IOException }
import scala.util.{ Try, Success, Failure }
import kr.ac.kaist.safe.config.{ Config, ConfigOption, OptionKind, BoolOption, StrOption }
import kr.ac.kaist.safe.ast_rewriter.{ Hoister, Disambiguator, WithRewriter }
import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.nodes.Program
import kr.ac.kaist.safe.util.{ NodeUtil, Useful }

// ASTRewrite phase
case class ASTRewrite(
    prev: Parse = Parse(),
    astRewriteConfig: ASTRewriteConfig = ASTRewriteConfig()
) extends Phase(Some(prev), Some(astRewriteConfig)) {
  override def apply(config: Config): Unit = rewrite(config) recover {
    case ex => Console.err.print(ex.toString)
  }
  def rewrite(config: Config): Try[Program] =
    prev.parse(config).flatMap(rewrite(config, _))
  def rewrite(config: Config, pgm: Program): Try[Program] = {
    // Rewrite AST.
    val hoister = new Hoister(pgm)
    var program = hoister.doit
    var excLog: ExcLog = hoister.excLog
    val disambiguator = new Disambiguator(program)
    program = disambiguator.result
    excLog += disambiguator.excLog
    val withRewriter: WithRewriter = new WithRewriter(program, false)
    program = withRewriter.doit
    excLog += withRewriter.excLog

    // Report errors.
    if (excLog.hasError) {
      println(NodeUtil.getFileName(program) + ":")
      println(excLog)
    }

    // Pretty print to file.
    astRewriteConfig.outFile match {
      case Some(out) => Useful.fileNameToWriters(out).map { pair =>
        {
          val ((fw, writer)) = pair
          writer.write(program.toString(0))
          writer.close; fw.close
          println("Dumped rewritten AST to " + out)
          program
        }
      }
      case None => Try(program)
    }
  }
}

// ASTRewrite phase helper.
object ASTRewrite extends PhaseHelper {
  def create: ASTRewrite = ASTRewrite()
}

// Config options for the ASTRewrite phase.
case class ASTRewriteConfig(
    var verbose: Boolean = false,
    var outFile: Option[String] = None
) extends ConfigOption {
  val prefix: String = "astRewrite:"
  val optMap: Map[String, OptionKind] = Map(
    "verbose" -> BoolOption(() => verbose = true),
    "out" -> StrOption((s: String) => outFile = Some(s))
  )
}
