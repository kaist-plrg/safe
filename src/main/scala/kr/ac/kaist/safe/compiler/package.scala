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
package kr.ac.kaist.safe

import kr.ac.kaist.safe.compiler.IndexCollector.IdxMap

/**
 * Created by ysko on 2018. 3. 16..
 */
package object compiler {
  implicit def any2waterfall[A](a: A): Object { def |>[B](f: (A) => B): B } = new AnyRef {
    def |>[B](f: A => B) = f(a)
  }

  def whenSome[A](o: Option[A])(F: A => IdxMap => IdxMap): IdxMap => IdxMap =
    whenSomeG(o)(F)(x => x)
  def whenSomeG[A, B](o: Option[A])(F: A => B)(D: B): B = {
    o match {
      case Some(s) => F(s)
      case _ => D
    }
  }
}
