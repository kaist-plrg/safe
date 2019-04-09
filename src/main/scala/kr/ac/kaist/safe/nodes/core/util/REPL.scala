/**
 * *****************************************************************************
 * Copyright (c) 2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.nodes.core

import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.nodes.core.Interp._
import kr.ac.kaist.safe.nodes.core.Parser._
import org.jline.builtins.Completers.TreeCompleter
import org.jline.builtins.Completers.TreeCompleter._
import org.jline.reader._
import org.jline.reader.impl._
import org.jline.terminal._
import org.jline.utils.InfoCmp.Capability
import org.jline.utils._
import scala.util.{ Try, Success, Failure }

// REPL
object REPL {
  def run(initial: State, detail: Boolean): Unit = {
    val cyan = "\u001b[36m"
    val reset = "\u001b[0m"
    val builder: TerminalBuilder = TerminalBuilder.builder()
    val terminal: Terminal = builder.build()
    val completer: TreeCompleter = new TreeCompleter(
      node("delete"),
      node("function"),
      node("return"),
      node("if"),
      node("while"),
      node("label"),
      node("break"),
      node("try"),
      node("throw"),
      node("assert"),
      node("print")
    )
    val reader: LineReader = LineReaderBuilder.builder()
      .terminal(terminal)
      .completer(completer)
      .build()
    val writer = terminal.writer()

    def clear: Unit = {
      print("\u001b[2J\u001b[1;1H")
    }
    def stopMessage(msg: String): Unit = {
      print(msg)
      System.console().reader().read
    }

    var st: State = initial
    def pre: String = "Instruction: " + st.insts.map(inst => LINE_SEP + "  " + beautify(inst, detail = detail)).mkString
    def prompt: String = pre + LINE_SEP + s"${cyan}core>${reset} "
    def fixMsg: String = pre + LINE_SEP + "Please press the enter key..."

    def fixpoint: Unit = {
      st.insts match {
        case inst :: rest =>
          // clear
          // stopMessage(fixMsg)
          st = interp(inst)(st.copy(insts = rest))
          fixpoint
        case Nil =>
      }
    }

    var keep: Boolean = true
    while (keep) {
      // clear screen
      // terminal.puts(Capability.clear_screen)

      // fixpoint
      fixpoint

      // reader
      try {
        reader.readLine(prompt) match {
          case null =>
          case "exit" => keep = false
          case line =>
            val inst = parseInst(line)
            st = interp(inst)(st)
            terminal.flush()
        }
      } catch {
        case e: EndOfFileException => keep = false
        case e: UserInterruptException => keep = false
        case e: java.lang.RuntimeException => stopMessage(s"Parsing failed..")
        case e: Throwable => stopMessage(s"ERROR: $e")
      }
    }
  }
}
