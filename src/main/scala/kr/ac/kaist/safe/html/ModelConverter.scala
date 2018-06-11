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

package kr.ac.kaist.safe.html

import kr.ac.kaist.safe.errors.error.ModelParseError
import kr.ac.kaist.safe.analyzer.domain._
import java.io._
import java.nio.charset.Charset

import scala.collection.immutable.HashMap
import scala.util.{ Try, Failure => Fail, Success => Succ }
import scala.util.parsing.combinator._
import kr.ac.kaist.safe.compiler._

import scala.language.reflectiveCalls
object JSModelE {
  val empty: JSModelE = JSModelE(Heap(HashMap()), HashMap.empty[FId, String])
}

case class JSModelE(heap: Heap, funcs: HashMap[FId, String]) {
  private def loc(s: Loc): String = {
    // remove recent tag.
    s.toString.substring(2)
  }

  private def iname(iv: IName): String = {
    iv match {
      case ICall => "@function"
      case IClass => "@class"
      case IConstruct => "@construct"
      case IExtensible => "@extensible"
      case IHasInstance => "@hasinstance"
      case IPrimitiveValue => "@primitive"
      case IPrototype => "@proto"
      case IScope => "@scope"
      case _ => throw new InternalError("TODO")
    }
  }

  private def addProp(s: String, o: Obj, iv: IName)(m: HashMap[FId, String]): HashMap[FId, String] = {
    if (o.containsi(iv)) {
      val t = iv match {
        case ICall => "fun"
        case IConstruct => "con"
        case _ => throw new InternalError("TODO")
      }
      o.lookupi(iv) match {
        case FId(i) => m + (FId(i) -> s"$s.$t")
        case _ => throw new InternalError("TODO")
      }
    } else m
  }

  def extract(path: String, out: String): Unit = {
    val path_write: String = new File(out).getAbsolutePath

    val map =
      (HashMap.empty[FId, String] /: heap) {
        case (m, (l, o)) =>
          val sloc = loc(l)
          m |> addProp(sloc, o, ICall) |> addProp(sloc, o, IConstruct)
      }

    map.foreach {
      case (i, name) =>
        System.out.println(s" * '$name.js'")
        write(s"$path_write/$name.js", funcs(i))
    }

    System.out.println(path_write)
  }

  private def write(name: String, body: String): Unit = {
    val file = new File(name)
    val fw = new FileWriter(file)
    val bs = new BufferedWriter(fw)
    bs.write(body)
    bs.close()
    fw.close()
  }
}

// Argument parser by using Scala RegexParsers.
object ModelConverter extends RegexParsers with JavaTokenParsers {
  def apply(code: String): Try[JSModelE] = {
    val sr = new StringReader(code)
    val in = new BufferedReader(sr)
    val result = parseModel(in)
    in.close()
    sr.close()
    result
  }
  def parseFile(fileName: String): Try[JSModelE] = {
    val fs = new FileInputStream(new File(fileName))
    val sr = new InputStreamReader(fs, Charset.forName("UTF-8"))
    val in = new BufferedReader(sr)
    val result = parseModel(in)
    in.close()
    sr.close()
    fs.close()
    result
  }

  def convertModels(files: List[String], out: String): Unit = {
    files.foreach { fileName =>
      val path = new File(fileName).getAbsolutePath
      val model = ModelConverter.parseFile(fileName).get
      model.extract(path, out)
    }
  }

  //////////////////////////////////////////////////////////////////////////
  // Private Helper Functions
  //////////////////////////////////////////////////////////////////////////
  // parse
  private def parseModel(reader: BufferedReader): Try[JSModelE] = {
    parse(jsModel, reader) match {
      case Success(result, _) => Succ(result)
      case fail @ NoSuccess(_, _) => Fail(ModelParseError(fail.toString))
    }
  }
  // repeat rules
  private def emptyList[T]: Parser[List[T]] = success(Nil)
  private def repsepE[T](p: => Parser[T], sep: String): Parser[List[T]] =
    p ~! (("," ~> repsepE(p, sep)) | emptyList) ^^ { case x ~ xs => x :: xs } | emptyList

  // primitive parser
  private lazy val num: Parser[Double] =
    floatingPointNumber ^^ { _.toDouble } |
      "NaN" ^^^ Double.NaN |
      "Infinity" ^^^ Double.PositiveInfinity |
      "-Infinity" ^^^ Double.NegativeInfinity
  private lazy val int: Parser[Int] = wholeNumber ^^ { _.toInt }
  private lazy val str: Parser[String] = "\"" ~> "[^\"]*".r <~ "\""
  private lazy val t: Parser[Boolean] = "true" ^^^ { true }
  private lazy val f: Parser[Boolean] = "false" ^^^ { false }
  private lazy val bool: Parser[Boolean] = t | f
  private lazy val T: Parser[Boolean] = "T" ^^^ { true }
  private lazy val F: Parser[Boolean] = "F" ^^^ { false }
  private lazy val shortBool: Parser[Boolean] = T | F
  private lazy val any: Parser[String] = """[^\\]*""".r

  // JavaScript primitive value
  private lazy val jsNum: Parser[Num] = num ^^ { Num(_) }
  private lazy val jsStr: Parser[Str] = str ^^ { Str }
  private lazy val jsNull: Parser[Null] = "null" ^^^ { Null }
  private lazy val jsBool: Parser[Bool] = bool ^^ { Bool }
  private lazy val jsShortBool: Parser[Bool] = shortBool ^^ { Bool }
  private lazy val jsShortBoolE: Parser[Bool] =
    jsShortBool | "" ~> failure("illegal start of boolean(T/F)")
  private lazy val jsUndef: Parser[Undef] = "undefined" ^^^ { Undef }
  private lazy val jsPValue: Parser[PValue] = jsNum | jsStr | jsNull | jsBool | jsUndef

  // JavaScript primitive type
  private lazy val jsStrT: Parser[StringT.type] = "string" ^^^ { StringT }
  private lazy val jsNumT: Parser[NumberT.type] = "number" ^^^ { NumberT }
  private lazy val jsBoolT: Parser[BoolT.type] = "bool" ^^^ { BoolT }
  private lazy val jsPrimType: Parser[Value] = jsStrT | jsNumT | jsBoolT

  // JavaScript value
  private lazy val jsLoc: Parser[Loc] = "#" ~> """[_\[\]0-9a-zA-Z.<>]+""".r ^^ { Loc(_) }
  private lazy val jsValue: Parser[Value] = jsPValue | jsLoc | jsPrimType
  private lazy val jsValueE: Parser[Value] = jsValue | failure("illegal start of value")

  // JavaScript data property
  private lazy val jsDataProp: Parser[DataProp] = "<" ~> (
    jsValueE ~
    ("," ~> jsShortBoolE) ~
    ("," ~> jsShortBoolE) ~
    ("," ~> jsShortBoolE)
  ) <~ ">" ^^ {
      case value ~ writable ~ enumerable ~ configurable =>
        DataProp(value, writable, enumerable, configurable)
    }

  // JavaScript internal property
  private lazy val jsIPrototype = "[[Prototype]]" ^^^ { IPrototype }
  private lazy val jsIClass = "[[Class]]" ^^^ { IClass }
  private lazy val jsIExtensible = "[[Extensible]]" ^^^ { IExtensible }
  private lazy val jsIPrimitiveValue = "[[PrimitiveValue]]" ^^^ { IPrimitiveValue }
  private lazy val jsICall = "[[Call]]" ^^^ { ICall }
  private lazy val jsIConstruct = "[[Construct]]" ^^^ { IConstruct }
  private lazy val jsIScope = "[[Scope]]" ^^^ { IScope }
  private lazy val jsIHasInstance = "[[HasInstance]]" ^^^ { IHasInstance }
  private lazy val jsIName: Parser[IName] = {
    jsIPrototype | jsIClass | jsIExtensible | jsIPrimitiveValue |
      jsICall | jsIConstruct | jsIScope | jsIHasInstance
  }
  private lazy val jsFId: Parser[FId] = "fun(" ~> int <~ ")" ^^ (n => FId(n))
  private lazy val jsIValue: Parser[IValue] = jsValue | jsFId
  private lazy val jsIValueE: Parser[IValue] =
    jsIValue | "" ~> failure("illegal start of IValue")

  // JavaScript object
  private type PMap = Map[String, DataProp]
  private type IMap = Map[IName, IValue]
  private def jsObjMapTuple: Parser[(PMap, IMap)] = {
    lazy val empty: Parser[(PMap, IMap)] = success((HashMap(), HashMap()))
    lazy val jsMember = (str <~ ":") ~! jsDataProp ^^ { case n ~ d => (n, d) }
    lazy val jsIMember = (jsIName <~ ":") ~! jsIValueE ^^ { case n ~ v => (n, v) }
    lazy val next = ("," ~> jsObjMapTuple) | empty
    jsMember ~! next ^^
      { case (name, dp) ~ ((pmap, imap)) => (pmap + (name -> dp), imap) } |
      jsIMember ~! next ^^
      { case (iname, iv) ~ ((pmap, imap)) => (pmap, imap + (iname -> iv)) } |
      empty
  }
  private lazy val jsObject: Parser[Obj] = "{" ~> jsObjMapTuple <~ "}" ^^ {
    case (pmap, imap) => Obj(pmap, imap)
  }

  // JavaScript Heap
  private lazy val jsHeap: Parser[Heap] = "{" ~> repsepE((jsLoc <~ ":") ~! jsObject, ",") <~ "}" ^^ {
    lst => Heap((HashMap.empty[Loc, Obj] /: lst) { case (map, loc ~ obj) => map + (loc -> obj) })
  }

  // JavaScript function
  private lazy val jsFun: Parser[String] = """[\\""" ~> any <~ """\\]""" ^^ { fun => fun }

  private lazy val jsFuncs: Parser[HashMap[FId, String]] = "{" ~> repsepE((int <~ ":") ~! jsFun, ",") <~ "}" ^^ {
    lst =>
      lst.foldLeft(HashMap.empty[FId, String]) {
        case (funcs, mid ~ func) => funcs + (FId(mid) -> func)
      }
  }

  // JavaScript model
  private lazy val jsModel: Parser[JSModelE] =
    ("Heap" ~> ":" ~> jsHeap) ~! ("Function" ~> ":" ~> jsFuncs) ^^ {
      case heap ~ map => JSModelE(heap, map)
    }
}
