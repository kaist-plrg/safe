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

import kr.ac.kaist.safe.analyzer.Semantics
import kr.ac.kaist.safe.config.{ BoolOption, Config, ConfigOption, NumOption, OptionKind, StrOption }
import kr.ac.kaist.safe.cfg_builder.CFG
import kr.ac.kaist.safe.analyzer.domain._

import scala.util.{ Success, Failure }

// Analyze phase struct.
case class Analyze(
    prev: CFGBuild = CFGBuild(),
    analyzeConfig: AnalyzeConfig = AnalyzeConfig()
) extends Phase(Some(prev), Some(analyzeConfig)) {
  override def apply(config: Config): Unit = analyze(config)
  def analyze(config: Config): Option[Int] = {
    prev.cfgBuild(config) match {
      case Success(cfg) => analyze(config, cfg)
      case Failure(_) => None
    }
  }

  def analyze(config: Config, cfg: CFG): Option[Int] = {
    //TODO: Below are temporal analyzer main code
    val utils = Utils(analyzeConfig.AbsUndef, analyzeConfig.AbsNull, analyzeConfig.AbsBool, analyzeConfig.AbsNumber, analyzeConfig.AbsString)
    val semantics = new Semantics(cfg, utils)
    None
  }
}

// Analyze phase helper.
object Analyze extends PhaseHelper {
  def create: Analyze = Analyze()
}

// Config options for Analyze phase.
case class AnalyzeConfig(
    var verbose: Boolean = false,
    var outFile: Option[String] = None,
    var AbsUndef: AbsUndefUtil = DefaultUndefUtil,
    var AbsNull: AbsNullUtil = DefaultNullUtil,
    var AbsBool: AbsBoolUtil = DefaultBoolUtil,
    var AbsNumber: AbsNumberUtil = DefaultNumUtil,
    var AbsString: AbsStringUtil = new DefaultStrSetUtil(0)
) extends ConfigOption {
  val prefix: String = "analyze:"
  val optMap: Map[String, OptionKind] = Map(
    "verbose" -> BoolOption(() => verbose = true),
    "out" -> StrOption((s: String) => outFile = Some(s)),
    "maxStrSetSize" -> NumOption((n: Int) => if (n > 0) AbsString = new DefaultStrSetUtil(n))
  )
}