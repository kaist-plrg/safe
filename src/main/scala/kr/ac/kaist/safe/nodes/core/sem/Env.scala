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

// CORE Environments
case class Env(
    globals: Map[Id, Value] = Map(),
    locals: Map[Id, Value] = Map(),
    labels: Map[Label, LabelCont] = Map(),
    retLabel: Option[ScopeCont] = None,
    excLabel: Option[ScopeCont] = None
) {
  // update identifiers
  def update(id: Id, value: Value): Env =
    copy(locals = locals + (id -> value))

  // update labels
  def update(label: Label, cont: LabelCont): Env =
    copy(labels = labels + (label -> cont))

  // update return label
  def updateRet(cont: ScopeCont): Env =
    copy(retLabel = Some(cont))

  // update exception label
  def updateExc(cont: ScopeCont): Env =
    copy(excLabel = Some(cont))

  // lookup identifiers
  def apply(id: Id): Value =
    locals.getOrElse(id, globals.getOrElse(id, error(s"free identifier: $id")))

  // lookup labels
  def apply(label: Label): LabelCont =
    labels.getOrElse(label, error(s"free label: $label"))
}
