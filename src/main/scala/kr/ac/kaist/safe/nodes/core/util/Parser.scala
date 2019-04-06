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

import java.io._
import java.nio.charset.Charset
import kr.ac.kaist.safe.errors.error.NotCoreFileError
import scala.util.Either
import scala.util.parsing.combinator.{ JavaTokenParsers, PackratParsers }

// parsers
object Parser extends JavaTokenParsers with PackratParsers {
  // treat comments as white spaces
  override protected val whiteSpace = """(\s|//.*)+""".r

  // parse a file into a program
  def fileToProgram(f: String): Program = {
    var fileName = new File(f).getCanonicalPath
    if (File.separatorChar == '\\') {
      // convert path string to linux style for windows
      fileName = fileName.charAt(0).toLower + fileName.replace('\\', '/').substring(1)
    }
    if (fileName.endsWith(".core")) {
      val fs = new FileInputStream(new File(f))
      val sr = new InputStreamReader(fs, Charset.forName("UTF-8"))
      val in = new BufferedReader(sr)
      val result = errHandle(parseAll(program, in))
      in.close; sr.close; fs.close
      result
    } else throw NotCoreFileError(fileName)
  }

  // parse files into a program
  def filesToProgram(fs: List[String]): Program = fs match {
    case files => (Program(Nil) /: files) {
      case (Program(l), f) =>
        val Program(r) = fileToProgram(f)
        Program(l ++ r)
    }
  }

  // parse with error message
  def errHandle[T <: CoreNode](result: ParseResult[T]): T = result match {
    case Success(result, _) => result
    case err => error(s"[CoreParser] $err")
  }

  // parse a String into a CoreNode
  def parseProgram(str: String): Program = errHandle(parseAll(program, str))
  def parseInst(str: String): Inst = errHandle(parseAll(inst, str))
  def parseExpr(str: String): Expr = errHandle(parseAll(expr, str))
  def parseRef(str: String): Ref = errHandle(parseAll(ref, str))
  def parseTy(str: String): Ty = errHandle(parseAll(ty, str))
  def parseUOp(str: String): UOp = errHandle(parseAll(uop, str))
  def parseBOp(str: String): BOp = errHandle(parseAll(bop, str))
  def parseValue(str: String): Value = errHandle(parseAll(value, str))

  ////////////////////////////////////////////////////////////////////////////////
  // Syntax
  ////////////////////////////////////////////////////////////////////////////////

  // programs
  lazy private val program: PackratParser[Program] = rep(inst) ^^ { Program(_) }

  // instructions
  type Prop = Either[Id, String]
  lazy private val iprop: PackratParser[(Prop, Expr)] = (id <~ ":") ~ expr ^^ { case x ~ e => (Left(x), e) }
  lazy private val prop: PackratParser[(Prop, Expr)] = (stringLiteral <~ ":") ~ expr ^^ { case str ~ e => (Right(str), e) }
  lazy private val props: PackratParser[List[(Prop, Expr)]] = "{" ~> repsep(iprop | prop, ",") <~ "}"
  lazy private val inst: PackratParser[Inst] = {
    "delete" ~> ref ^^ { IDelete(_) } |
      "return" ~> expr ^^ { case e => IReturn(e) } |
      ("if" ~> expr) ~ inst ~ ("else" ~> inst?) ^^ { case c ~ t ~ e => IIf(c, t, e.getOrElse(ISeq(Nil))) } |
      ("while" ~> expr) ~ inst ^^ { case c ~ b => IWhile(c, b) } |
      "throw" ~> expr ^^ { case e => IThrow(e) } |
      "{" ~> rep(inst) <~ "}" ^^ { case seq => ISeq(seq) } |
      "assert" ~> expr ^^ { case e => IAssert(e) } |
      "print" ~> expr ^^ { case e => IPrint(e) } |
      "???" ^^^ { INotYetImpl } |
      (ref <~ "=") ~ ("try" ~> inst) ^^ { case x ~ i => ITry(x, i) } |
      (ref <~ "=" <~ "new") ~ (ty) ^^ { case x ~ t => IAlloc(x, t) } |
      (ref <~ "=") ~ ty ~ (props) ^^ {
        case x ~ t ~ props => ISeq(IAlloc(x, t) :: props.map {
          case (Left(id), e) => IExpr(RefIdProp(x, id), e)
          case (Right(str), e) => IExpr(RefStrProp(x, EStr(str.substring(1, str.length - 1))), e)
        })
      } |
      (ref <~ "=") ~ expr ~ ("(" ~> (repsep(expr, ",") <~ ")")) ^^ { case x ~ f ~ as => IApp(x, f, as) } |
      (ref <~ "=") ~ (expr) ^^ { case x ~ e => IExpr(x, e) }
  }

  // expressions
  lazy private val expr: PackratParser[Expr] = {
    floatingPointNumber ^^ { case s => ENum(s.toDouble) } |
      "i(0|-?[1-9]\\d*)".r ^^ { case s => EINum(s.substring(1, s.length).toLong) } |
      stringLiteral ^^ { case s => EStr(s.substring(1, s.length - 1)) } |
      "true" ^^^ EBool(true) |
      "false" ^^^ EBool(false) |
      "undefined" ^^^ EUndef |
      "null" ^^^ ENull |
      "(" ~> (uop ~ expr) <~ ")" ^^ { case u ~ e => EUOp(u, e) } |
      "(" ~> (bop ~ expr ~ expr) <~ ")" ^^ { case b ~ l ~ r => EBOp(b, l, r) } |
      "(" ~> ("?" ~> ref) <~ ")" ^^ { case ref => EExist(ref) } |
      "(" ~> ("typeof" ~> expr) <~ ")" ^^ { case e => ETypeOf(e) } |
      ("(" ~> repsep(id, ",") <~ ")") ~ ("=>" ~> inst) ^^
      { case ps ~ b => EFunc(ps, b) } |
      ref ^^ { ERef(_) }
  }

  // references
  lazy private val ref: PackratParser[Ref] = {
    ref ~ ("." ~> id) ^^ { case x ~ i => RefIdProp(x, i) } |
      ref ~ ("[" ~> expr <~ "]") ^^ { case x ~ e => RefStrProp(x, e) } |
      id ^^ { RefId(_) }
  }

  // types
  lazy private val ty: PackratParser[Ty] = ident ^^ { Ty(_) }

  // identifiers
  lazy private val id: PackratParser[Id] = ident ^^ { Id(_) }

  // unary operators
  lazy private val uop: PackratParser[UOp] = {
    "-" ^^^ ONeg | "!" ^^^ ONot | "~" ^^^ OBNot
  }

  // binary operators
  lazy private val bop: PackratParser[BOp] = {
    "+" ^^^ OPlus |
      "-" ^^^ OSub |
      "*" ^^^ OMul |
      "/" ^^^ ODiv |
      "%" ^^^ OMod |
      "=" ^^^ OEq |
      "&" ^^^ OBAnd |
      "|" ^^^ OBOr |
      "^" ^^^ OBXOr |
      "<<" ^^^ OLShift |
      "<" ^^^ OLt |
      ">>>" ^^^ OURShift |
      ">>" ^^^ OSRShift
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Values
  ////////////////////////////////////////////////////////////////////////////////
  // values
  lazy private val value: PackratParser[Value] = {
    ("(" ~> repsep(id, ",") <~ ")") ~ ("=>" ~> inst) ^^ { case ps ~ b => Func(ps, b) } |
      addr |
      floatingPointNumber ^^ { case n => Num(n.toDouble) } |
      "i" ~> wholeNumber ^^ { case n => INum(n.toLong) } |
      stringLiteral ^^ { Str(_) } |
      "true" ^^^ { Bool(true) } |
      "false" ^^^ { Bool(false) } |
      "undefined" ^^^ Undef |
      "null" ^^^ Null
  }

  // addresses
  lazy private val addr: PackratParser[Addr] = {
    "#addr(" ~> wholeNumber <~ ")" ^^ { case s => DynamicAddr(s.toLong) } |
      "#" ~> ident ^^ { NamedAddr(_) }
  }
}
