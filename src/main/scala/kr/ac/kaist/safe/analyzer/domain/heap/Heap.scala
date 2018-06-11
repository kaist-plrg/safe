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

package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.analyzer.HeapParser._
import scala.collection.immutable.HashMap
import scala.io.Source
import spray.json._

// concrete heap type
case class Heap(map: Map[Loc, Obj]) {
  def +(other: Heap): Heap = {
    val emptyObj = Obj.empty
    val newHeapMap = other.map.foldLeft(this.map) {
      case (map, (loc, obj)) => {
        val newObj = map.getOrElse(loc, emptyObj) + obj
        map + (loc -> newObj)
      }
    }
    Heap(newHeapMap)
  }
  def update(l: Loc, o: Obj): Heap = {
    Heap(map + (l -> o))
  }
  def foreach(f: ((Loc, Obj)) => Unit): Unit = map.foreach(f)
  def foldLeft[B](z: B)(op: (B, (Loc, Obj)) => B): B = {
    var result = z
    this foreach (x => result = op(result, x))
    result
  }
  def /:[B](z: B)(op: (B, (Loc, Obj)) => B): B = foldLeft(z)(op)
}

object Heap {
  val empty: Heap = Heap(HashMap.empty[Loc, Obj])
  def parse(fileName: String): Heap =
    Source.fromFile(fileName)("UTF-8").mkString.parseJson.convertTo
}
