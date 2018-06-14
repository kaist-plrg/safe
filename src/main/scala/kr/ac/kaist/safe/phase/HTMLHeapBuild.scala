/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase

import scala.util.{ Try, Success, Failure }
import kr.ac.kaist.safe.{ LINE_SEP, SafeConfig }
import kr.ac.kaist.safe.cfg_builder.DotWriter
import kr.ac.kaist.safe.nodes.cfg.CFG
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.models.JSModel
import kr.ac.kaist.safe.errors.error.NoChoiceError
import kr.ac.kaist.safe.html.HTMLModel

// HeapBuild phase
case object HTMLHeapBuild extends PhaseObj[(HTMLModel.T, CFG), HeapBuildConfig, (CFG, Semantics, HeapBuildConfig, Int)] {
  val name: String = "HTMLHeapBuilder"
  val help: String = "Build an initial heap including DOM model."

  def apply(
    in: (HTMLModel.T, CFG),
    safeConfig: SafeConfig,
    config: HeapBuildConfig
  ): Try[(CFG, Semantics, HeapBuildConfig, Int)] = {
    val (html, cfg) = in
    // initialization
    register(
      config.AbsUndef,
      config.AbsNull,
      config.AbsBool,
      config.AbsNum,
      config.AbsStr,
      DefaultLoc,
      config.aaddrType
    )

    // Note: jsModel must be true. We do not support the case that jsModel is not true so far.
    var initSt = Initialize(cfg, jsModel = true, Some(HTMLModel))
    initSt = HTMLModel.init(html, initSt)

    // handling snapshot mode
    config.snapshot.foreach(str =>
      initSt = Initialize.addSnapshot(initSt, str))

    val sens =
      CallSiteSensitivity(config.callsiteSensitivity) * CAPartition.DefaultCAPartitioning
    val initTP = sens.initTP
    val entryCP = ControlPoint(cfg.globalFunc.entry, initTP)

    val worklist = Worklist(cfg)
    worklist.add(entryCP)

    val sem = Semantics(cfg, worklist, Some(html))
    sem.setState(entryCP, initSt)

    Success((cfg, sem, config, -1))
  }

  def defaultConfig: HeapBuildConfig = HeapBuildConfig()
  val options: List[PhaseOption[HeapBuildConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during heap building are muted."),
    ("maxStrSetSize", NumOption((c, n) => if (n > 0) c.AbsStr = StringSet(n)),
      "the analyzer will use the AbsStr Set domain with given size limit n."),
    ("aaddrType", StrOption((c, s) => s match {
      case "normal" => c.aaddrType = NormalAAddr
      case "recency" => c.aaddrType = RecencyAAddr
      case str => throw NoChoiceError(s"there is no address abstraction type with name '$str'.")
    }), "address abstraction type."),
    ("callsiteSensitivity", NumOption((c, n) => if (n >= 0) c.callsiteSensitivity = n),
      "{number}-depth callsite-sensitive analysis will be executed."),
    ("loopSensitivity", NumOption((c, n) => if (n >= 0) c.loopSensitivity = n),
      "{number}-depth loop-sensitive analysis will be executed."),
    ("snapshot", StrOption((c, s) => c.snapshot = Some(s)),
      "analysis with an initial heap generated from a dynamic snapshot(*.json)."),
    ("number", StrOption((c, s) => s match {
      case "default" => c.AbsNum = DefaultNumber
      // TODO case "flat" => c.AbsNum = FlatNumber
      case str => throw NoChoiceError(s"there is no abstract number domain with name '$str'.")
    }), "analysis with a selected number domain."),
    ("jsModel", BoolOption(c => c.jsModel = true),
      "analysis with JavaScript models.")
  )

  // cache for JS model
  var jscache: Option[JSModel] = None
}

