/**
 * *****************************************************************************
 * Copyright (c) 2016-2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.console

import java.io.PrintWriter

import jline.console.ConsoleReader
import jline.console.completer._
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.analyzer.console.command._
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.phase.HeapBuildConfig

import scala.collection.JavaConverters._

class Console(
    override val cfg: CFG,
    override val sem: Semantics,
    override val config: HeapBuildConfig,
    var iter0: Int
) extends Interactive {
  ////////////////////////////////////////////////////////////////
  // private variables
  ////////////////////////////////////////////////////////////////

  private val reader = new ConsoleReader()
  private val out: PrintWriter = new PrintWriter(reader.getOutput)

  iter = iter0
  init()

  ////////////////////////////////////////////////////////////////
  // API
  ////////////////////////////////////////////////////////////////

  override def runFixpoint(): Unit = {
    val prepare = prepareToRunFixpoint
    lazy val (st, excSt) = getResult
    val alreadyVisited = (debugMode || stopAlreadyVisited) && (visited contains cur)
    val exitExc = (debugMode || stopExitExc) && (!excSt.isBottom)
    val oneSideBot = debugMode && (
      (st.heap.isBottom && !st.context.isBottom) ||
      (st.context.isBottom && !st.heap.isBottom) ||
      (excSt.heap.isBottom && !excSt.context.isBottom) ||
      (excSt.context.isBottom && !excSt.heap.isBottom)
    )
    if (prepare || alreadyVisited || exitExc || oneSideBot) {
      if (alreadyVisited) println("[STOP] already visited CFGBlock.")
      if (exitExc) println("[STOP] it creates exceptions.")
      if (oneSideBot) println("[STOP] it create one side bottom.")
      if (showIter && startTime != beforeTime) {
        val duration = System.currentTimeMillis - startTime
        println(s"total: $duration ms")
      }
      setPrompt()
      while ({
        println
        val line = reader.readLine
        startTime = System.currentTimeMillis
        beforeTime = System.currentTimeMillis
        val loop = runCmd(line) match {
          case CmdResultContinue(o) =>
            println(o)
            true
          case CmdResultBreak(o) =>
            println(o)
            false
          case CmdResultRestart =>
            prepareToRunFixpoint
            setPrompt()
            true
        }
        out.flush()
        loop
      }) {}
    } else if (showIter) {
      val curTime = System.currentTimeMillis
      val duration = curTime - beforeTime
      if (duration > INTERVAL) {
        println(s"Iter[$iter]: $duration ms")
        beforeTime = curTime
      }
    }
    visited += cur
  }
  override def goHome(): Unit = {
    if (cur == home) {
      println("* here is already original control point.")
    } else {
      cur = home
      println("* reset the current control point.")
      setPrompt()
    }
  }
  def moveCurCP(block: CFGBlock): Unit = {
    val tpList: List[TracePartition] = sem.getState(block).toList.map {
      case (tp, _) => tp
    }
    val len: Int = tpList.length
    val fid: Int = block.func.id
    tpList match {
      case Nil => println(s"* no call-context in function[$fid] $block")
      case tp :: Nil => {
        cur = ControlPoint(block, tp)
        println(s"* current control point changed.")
        setPrompt()
      }
      case _ =>
        setPrompt(
          tpList.zipWithIndex.map {
            case (tp, idx) => s"[$idx] $tp" + LINE_SEP
          }.mkString + s"select call context index > "
        )
        while ({
          println
          reader.readLine match {
            case null =>
              println
              println("* current control point not changed.")
              false
            case "" => true
            case line => !line.forall(_.isDigit) || (line.toInt match {
              case idx if idx < len =>
                cur = ControlPoint(block, tpList(idx))
                println(s"* current control point changed.")
                false
              case _ => println(s"* given index is out of bound $len"); true
            })
          }
        }) {}
        setPrompt()
    }
  }

  override def getPrompt: String = {
    val block = cur.block
    val fname = block.func.simpleName
    val fid = block.func.id
    val span = block.span
    val tp = cur.tracePartition
    val tpStr = if (tp == EmptyTP) "" else s", $tp"
    s"<$fname[$fid]: $block$tpStr> @$span $LINE_SEP Iter[$iter] > "
  }

  ////////////////////////////////////////////////////////////////
  // private helper
  ////////////////////////////////////////////////////////////////

  private def init(): Unit = {
    val cmds = Command.commands.map(_.name).asJavaCollection
    reader.addCompleter(new StringsCompleter(cmds))
    // TODO extend aggregator for sub-command
    // reader.addCompleter(new AggregateCompleter(
    //   new ArgumentCompleter(new StringsCompleter("asdf"), new StringsCompleter("sdf"), new NullCompleter()),
    //   new ArgumentCompleter(new StringsCompleter("wer"), new NullCompleter())
    // ))
    runCmd("help") match {
      case o: CmdResult => println(o)
    }
  }

  private def setPrompt(prompt: String = getPrompt): Unit = {
    reader.setPrompt(prompt)
  }
}
