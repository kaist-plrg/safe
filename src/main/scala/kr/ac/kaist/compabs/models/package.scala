/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs

import kr.ac.kaist.compabs.models.adomain._

package object models {
  implicit def any2waterfall[A](a: A): Object { def |>[B](f: (A) => B): B } = new AnyRef {
    def |>[B](f: A => B) = f(a)
  }

  val DLoc: AbsDOMLoc = ILoc.Make()
  val DLocSet = ILocSet.Make(DLoc)
  val DNode: AbsDOMNode = DOMNode.Make()
  val DHeap: AbsDOMHeap = DOMHeap.Make()
}
