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
    ids: Map[Id, Value] = Map(),
    labels: Map[Label, Cont] = Map(),
    retLabel: Option[(Id, Cont)] = None,
    excLabel: Option[(Id, Cont)] = None
) {
  // update identifiers
  def update(id: Id, value: Value): Env =
    copy(ids = ids + (id -> value))

  // update labels
  def update(label: Label, cont: Cont): Env =
    copy(labels = labels + (label -> cont))

  // update return label
  def updateRet(id: Id, cont: Cont): Env =
    copy(retLabel = Some((id, cont)))

  // update exception label
  def updateExc(id: Id, cont: Cont): Env =
    copy(excLabel = Some((id, cont)))

  // lookup identifiers
  def apply(id: Id): Value =
    ids.getOrElse(id, error(s"free identifier: $id"))

  // lookup labels
  def apply(label: Label): Cont =
    labels.getOrElse(label, error(s"free label: $label"))
}
