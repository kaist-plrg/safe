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

package kr.ac.kaist.safe.analyzer

import kr.ac.kaist.safe.nodes.cfg._

import scala.collection.immutable.{ HashMap, HashSet, LongMap }

case class Worklist(cfg: CFG) {
  def init(entryCP: ControlPoint): Unit = {
    worklist = empty
    add(entryCP)
  }

  private lazy val emptySet = HashSet.empty[ControlPoint]
  private lazy val empty: LongMap[HashSet[ControlPoint]] = LongMap.empty[HashSet[ControlPoint]]
  // work list
  private var worklist: LongMap[HashSet[ControlPoint]] = empty

  def getWorklist: HashSet[ControlPoint] =
    (HashSet.empty[ControlPoint] /: worklist)((set_i, kv) => set_i ++ kv._2)

  // order map for blocks
  private val orderMap: Map[CFGBlock, Long] = {
    val cfgBlockList: List[CFGBlock] = cfg.getAllBlocks.sortWith((b1, b2) => {
      if (b1.func.id == b2.func.id) compareBlockType(b1, b2)
      else b1.func.id < b2.func.id
    })
    val (orderMap, _) = cfgBlockList.foldLeft((HashMap[CFGBlock, Long](), 0L)) {
      case ((tmpMap, tmpOrder), block) =>
        (tmpMap + (block -> tmpOrder), tmpOrder + 1)
    }
    orderMap
  }

  // get order of block
  private def getOrder(block: CFGBlock): Long = orderMap.getOrElse(block, 0L)

  // compare types of blocks
  def compareBlockType(b1: CFGBlock, b2: CFGBlock): Boolean =
    (b1, b2) match {
      case (Entry(_), _) | (Exit(_), ExitExc(_)) => true
      case (_, Entry(_)) | (ExitExc(_), Exit(_)) => false
      case (_, Exit(_)) | (_, ExitExc(_)) => true
      case (Exit(_), _) | (ExitExc(_), _) => false
      case _ => b1.id < b2.id
    }

  // add control point to work list
  def add(cp: ControlPoint): Unit = {
    val order = getOrder(cp.block)
    val ocp = worklist.getOrElse(order, emptySet)
    worklist = worklist + (order -> (ocp + cp))
  }

  def isEmpty: Boolean = worklist.isEmpty

  def pop: Set[ControlPoint] = {
    val head = worklist.head
    worklist = worklist.tail
    head._2
  }

  def head: Set[ControlPoint] = worklist.head._2

  def getOrderMap: Map[CFGBlock, Long] = orderMap

  override def toString: String = {
    worklist.map(work => work.toString).mkString(", ")
  }

  def has(block: CFGBlock): Boolean = worklist.values.exists(_.exists(_.block == block))
}
