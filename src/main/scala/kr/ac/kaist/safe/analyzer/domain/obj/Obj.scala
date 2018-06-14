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

import kr.ac.kaist.safe.errors.error.INameParseError

import scala.collection.immutable.HashMap
import spray.json._

// concrete object type
case class Obj(nmap: Map[String, DataProp], imap: Map[IName, IValue]) {
  def +(other: Obj): Obj = {
    val newnmap = this.nmap.foldLeft(other.nmap) {
      case (map, (str, prop)) => {
        map.get(str) match {
          case Some(p) => map + (str -> (prop + p))
          case None => map + (str -> prop)
        }
      }
    }
    val newimap = this.imap.foldLeft(other.imap) {
      case (map, (name, value)) => {
        map.get(name) match {
          case Some(v) => map + (name -> v)
          case None => map + (name -> value)
        }
      }
    }
    Obj(newnmap, newimap)
  }
  def update(name: String, dp: DataProp): Obj = {
    copy(nmap = nmap + (name -> dp))
  }
  def updatei(name: IName, dp: IValue): Obj = {
    copy(imap = imap + (name -> dp))
  }
  def lookup(name: String): DataProp = nmap(name)
  def lookupi(name: IName): IValue = imap(name)
  def containsi(in: IName): Boolean = imap.keySet.contains(in)
}

object Obj {
  val empty: Obj = Obj(HashMap.empty[String, DataProp], HashMap.empty[IName, IValue])
}

// internal property names
sealed abstract class IName(name: String) {
  override def toString: String = s"[[$name]]"
  def toJson: JsValue = JsString(name)
}
object IName {
  val all: List[IName] = List(
    IPrototype,
    IClass,
    IExtensible,
    IPrimitiveValue,
    ICall,
    IConstruct,
    IScope,
    IHasInstance,
    ITargetFunction,
    IBoundThis,
    IBoundArgs,
    IInterval,
    INode,
    IStyle,
    IChildNodes,
    IAttributes,
    ILength
  )
  def makeMap[V](value: V): Map[IName, V] = all.foldLeft(HashMap[IName, V]()) {
    case (map, iname) => map + (iname -> value)
  }
  def fromJson(v: JsValue): IName = v match {
    case JsString("Prototype") => IPrototype
    case JsString("Class") => IClass
    case JsString("Extensible") => IExtensible
    case JsString("PrimitiveValue") => IPrimitiveValue
    case JsString("Call") => ICall
    case JsString("Construct") => IConstruct
    case JsString("Scope") => IScope
    case JsString("HasInstance") => IHasInstance
    case JsString("TargetFunction") => ITargetFunction
    case JsString("BoundThis") => IBoundThis
    case JsString("BoundArgs") => IBoundArgs
    case _ => throw INameParseError(v)
  }
}
case object IPrototype extends IName("Prototype")
case object IClass extends IName("Class")
case object IExtensible extends IName("Extensible")
case object IPrimitiveValue extends IName("PrimitiveValue")
case object ICall extends IName("Call")
case object IConstruct extends IName("Construct")
case object IScope extends IName("Scope")
case object IHasInstance extends IName("HasInstance") //TODO
case object ITargetFunction extends IName("TargetFunction")
case object IBoundThis extends IName("BoundThis")
case object IBoundArgs extends IName("BoundArgs")
case object IInterval extends IName("Interval")
case object INode extends IName("Node")
case object IStyle extends IName("Style")
case object IChildNodes extends IName("StyleNodes")
case object IAttributes extends IName("Attributes")
case object ILength extends IName("Length")
