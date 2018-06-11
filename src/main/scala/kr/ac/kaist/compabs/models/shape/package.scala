/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.shape

import java.io._

import kr.ac.kaist.compabs.models.cdomain._
import kr.ac.kaist.compabs.models.parser.Shape

import scala.collection.immutable.{ HashMap, HashSet }
import scala.collection.mutable

case class HUPropValue(writable: Option[Boolean], enumerable: Option[Boolean], configurable: Option[Boolean], value: Option[CValue], setter: Option[CLoc], getter: Option[CLoc], gs: Option[CValue]) extends Serializable {
  private[this] def optBool(b: Option[Boolean]) = {
    b match {
      case (Some(true)) => "T"
      case (Some(false)) => "F"
      case _ => " "
    }
  }
  override def toString: String = {
    s"${optBool(writable)}${optBool(enumerable)}${optBool(configurable)} $value $setter $getter $gs"
  }
}
case class HIPropValue(value: CValue) extends Serializable {
  override def toString: String = value.toString
}

sealed trait HProp extends Serializable {
  def toString(indent: Int): String
}
case class HUProp(name: String, value: HUPropValue) extends HProp {
  def toString(indent: Int = 0): String = " " * indent + s"$name U=> $value"
}
case class HIProp(name: String, value: HIPropValue) extends HProp {
  def toString(indent: Int = 0): String = " " * indent + s"$name I=> $value"
}

case class HObject(loc: CLoc, properties: List[HProp]) extends Serializable {
  override def toString: String = {
    val l = s"* ${loc.toString}"
    val list = properties.map(_.toString(2))
    val v = l :: list
    v.mkString("\n")
  }
}

case class Host(list: List[HObject]) extends Serializable

object Host {
  class ModelError(msg: String) extends RuntimeException(msg)

  val locGlobal = "#Global"
  def reducing(h: Host): Host = {
    // Filters out all the unused HObjects and print them as warnings.
    val usedSet = mutable.HashSet[String](locGlobal)
    var worklist = HashSet[String](locGlobal)
    val map = h.list.foldLeft(HashMap[String, List[HProp]]())((m, o) => {
      if (m.contains(o.loc.loc)) System.err.println("* Warning: a location must be unique for each object")
      m + (o.loc.loc -> o.properties)
    })

    def check(loc: String): Unit = {
      if (!usedSet.contains(loc)) {
        usedSet += loc
        worklist = worklist + loc
      }
    }
    while (worklist.nonEmpty) {
      val w = worklist.head
      worklist = worklist.tail
      map.get(w) match {
        case Some(l) => l.foreach {
          case HUProp(_, HUPropValue(_, _, _, Some(CLoc(loc: String)), _, _, _)) => check(loc)
          case HUProp(_, HUPropValue(_, _, _, _, Some(CLoc(loc1)), Some(CLoc(loc2)), _)) =>
            check(loc1); check(loc2)
          case HUProp(_, HUPropValue(_, _, _, _, Some(CLoc(loc1)), _, _)) => check(loc1)
          case HUProp(_, HUPropValue(_, _, _, _, _, Some(CLoc(loc2)), _)) => check(loc2)
          case HIProp(cp, HIPropValue(CLoc(loc))) if cp == "@function" =>
          case HIProp(cp, HIPropValue(CLoc(loc))) if cp == "@construct" =>
          case HIProp(_, HIPropValue(CLoc(loc))) => check(loc)
          case _ => ()
        }
        case None => ()
      }
    }

    //    h.list.filterNot(o => usedSet.contains(o.loc.loc)).foreach(o => {
    //      System.err.println("* Warning: unused object: " + o.loc.loc)
    //    })
    Host(h.list.filter(o => usedSet.contains(o.loc.loc)))
  }

  def statistics(h: Host): Unit = {
    System.out.println("* # of host objects: " + h.list.length)
  }

  private def serialize(file: String, host: Host): Unit = {
    val fos = new FileOutputStream(file)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(host)
    oos.close()
    fos.close()
  }

  private def serialize_load(file: String, opt: String): Option[Host] = {
    val n = new java.io.File(opt)
    if (n.exists()) {
      // load opt
      val o = new java.io.File(file)
      if (o.exists() && n.lastModified() < o.lastModified()) {
        System.out.println("* Host file is recently updated. Discard the existing cache data.")
        None
      } else {
        val fis = new FileInputStream(opt)
        class ObjectInputStreamWithCustomClassLoader(fileInputStream: FileInputStream)
            extends ObjectInputStream(fileInputStream) {
          override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
            try {
              Class.forName(desc.getName, false, getClass.getClassLoader)
            } catch {
              case _: ClassNotFoundException => super.resolveClass(desc)
            }
          }
        }
        val ois = new ObjectInputStreamWithCustomClassLoader(fis)
        try {
          Some(ois.readObject().asInstanceOf[Host])
        } catch {
          case _: Throwable =>
            //          System.err.println("* Info: Invalidate the existing optimized data due to the code version difference.")
            None
        }
      }
    } else {
      None
    }
  }

  def load(path: String, name: String): Host = {
    val file = s"$path$name"
    //    System.out.println(s"* Loading a model: " + file)

    val opt = s"$file.opt"
    serialize_load(file, opt) match {
      case Some(host) => host
      case None =>
        val pp = new Shape(new FileReader(file), file)
        val res = pp.pHost(0)
        if (res.hasValue) {
          val list: List[HObject] = res.semanticValue()
          val host = Host(list)
          //          System.out.println("* Info: This is the first load of host object. Generating the optimized shape information.")
          serialize(opt, host) // Note: Due to the #BoundFunctionHelper, which is a dangling location, we should not use Host.reducing(host)
          host
        } else {
          val loc = pp.location(res.index)
          System.err.println(res.parseError().msg + " at " + loc.line + ":" + loc.column)
          throw new ModelError("cannot find heap.shape")
        }
    }
  }
}
