/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.html

import org.w3c.dom._
import org.w3c.dom.html._

import scala.collection.immutable.{ HashMap, HashSet }
import scala.collection.mutable
import scala.language.reflectiveCalls
import kr.ac.kaist.compabs.models.html.parser.{ CybernekoParser, HTMLParser }
import kr.ac.kaist.compabs.models.shape.Host
import kr.ac.kaist.compabs.models.shape.Host.ModelError
import kr.ac.kaist.compabs.models._
import kr.ac.kaist.compabs.models.cdomain.CValue

trait IChrome55 extends DOMModel {
  val name: String

  override val Parser: HTMLParser = CybernekoParser
  val appCodeName: String = "Mozilla"
  val appName: String = "Netscape"
  val appVersion: String = "5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.95 Safari/537.36"
  val userAgent: String = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.95 Safari/537.36"
  val compatMode: String = "CSS1Compat"
  val platform: String = "MacIntel" // "MacIntel", "Win32", "FreeBSD i386", "WebTV OS"
  val product: String = "Gecko"

  // wrapper type
  type SemanticsFun = (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue)
  type AIName
  def ii(s: String): AIName

  trait SFInputT {
    def copyi(state: AAbsState): SFInput
    val state: AAbsState
    def getArg(i: Int): AAbsValue
    def lenArg: AAbsNum
    def newAlloc(j: Long): ALoc
    def newRecentAlloc(j: Long): ALoc
    def newDAlloc(j: Long): DLoc.T
    val lset_this: AAbsLoc
    val v_this: AAbsValue
    val id: Long
  }
  type SFInput <: SFInputT

  def genAPI(i: SFInput => (AAbsState, AAbsState, AAbsValue)): SemanticsFun
  def nnAPI(name: String): SemanticsFun
  def returnValueC(v: AAbsValue)(s: SFInput): (AAbsState, AAbsState, AAbsValue)
  def returnValueD(v: AAbsValue, es: AAbsState)(s: SFInput): (AAbsState, AAbsState, AAbsValue)
  def returnValueAPI(v: AAbsValue): SemanticsFun
  def returnValueAPI(b: Boolean): SemanticsFun
  def returnValueAPI(s: String): SemanticsFun
  def returnValueLocName(s: String): SemanticsFun
  def returnValueIRange(l: Int, u: Int): SemanticsFun
  def returnUndef(si: SFInput): (AAbsState, AAbsState, AAbsValue)
  def toDOMString(v: AAbsValue): HashSet[String]

  def exception(es: Set[AException])(s: AAbsState): AAbsState

  // Note: Exceptional case: "float" -> "cssFloat"
  val cssTocssom: HashMap[String, String] = HashMap("background" -> "background", "background-attachment" -> "backgroundAttachment", "background-color" -> "backgroundColor",
    "background-image" -> "backgroundImage", "background-position" -> "backgroundPosition", "background-repeat" -> "backgroundRepeat", "border" -> "border",
    "border-bottom" -> "borderBottom", "border-bottom-color" -> "borderBottomColor", "border-bottom-style" -> "borderBottomStyle", "border-bottom-width" -> "borderBottomWidth",
    "border-color" -> "borderColor", "border-left" -> "borderLeft", "border-left-color" -> "borderLeftColor", "border-left-style" -> "borderLeftStyle",
    "border-left-width" -> "borderLeftWidth", "border-right" -> "borderRight", "border-right-color" -> "borderRightColor", "border-right-style" -> "borderRightStyle",
    "border-right-width" -> "borderRightWidth", "border-style" -> "borderStyle", "border-top" -> "borderTop", "border-top-color" -> "borderTopColor",
    "border-top-style" -> "borderTopStyle", "border-top-width" -> "borderTopWidth", "border-width" -> "borderWidth", "clear" -> "clear", "clip" -> "clip",
    "color" -> "color", "cursor" -> "cursor", "display" -> "display", "filter" -> "filter", "font" -> "font", "font-family" -> "fontFamily", "font-size" -> "fontSize",
    "font-variant" -> "fontVariant", "font-weight" -> "fontWeight", "height" -> "height", "left" -> "left", "letter-spacing" -> "letterSpacing", "line-height" -> "lineHeight",
    "list-style" -> "listStyle", "list-style-image" -> "listStyleImage", "list-style-position" -> "listStylePosition", "list-style-type" -> "listStyleType",
    "margin" -> "margin", "margin-bottom" -> "marginBottom", "margin-left" -> "marginLeft", "margin-right" -> "marginRight", "margin-top" -> "marginTop", "overflow" -> "overflow",
    "opacity" -> "opacity", "padding" -> "padding", "padding-bottom" -> "paddingBottom", "padding-left" -> "paddingLeft", "padding-right" -> "paddingRight", "padding-top" -> "paddingTop",
    "page-break-after" -> "pageBreakAfter", "page-break-before" -> "pageBreakBefore", "position" -> "position", "float" -> "cssFloat", "text-align" -> "textAlign",
    "text-decoration" -> "textDecoration", "text-indent" -> "textIndent", "text-transform" -> "textTransform", "top" -> "top", "vertical-align" -> "verticalAlign",
    "visibility" -> "visibility", "width" -> "width", "zoom" -> "zoom", "z-index" -> "zIndex")

  val AError: AException
  val ATypeError: AException
  val ADOMError: AException
  val AExceptionBot = HashSet.empty[AException]
  val eventHandlers: HashSet[String] = HashSet("onclick", "onkeydown", "onload", "onfocus")
  private[this] def isEventHandler(name: String): Boolean = eventHandlers.contains(name)

  var owner: Document = _
  var htmlDoc: Option[AAbsValue] = None
  var htmlHTML: Option[AAbsValue] = None
  var htmlHead: Option[AAbsValue] = None
  var htmlTitle: Option[AAbsValue] = None
  var htmlBody: Option[AAbsValue] = None
  val docName: String = "#document"
  val docHTMLName: String = "#documentHTML"
  val docHeadName: String = "#documentHead"
  val docTitleName: String = "#documentTitle"
  val docBodyName: String = "#documentBody"

  sealed trait NamedNode
  case object DocumentNode extends NamedNode
  case object DocumentElementNode extends NamedNode
  case object HeadNode extends NamedNode
  case object BodyNode extends NamedNode
  case object TitleNode extends NamedNode

  val AAbsValueNull: AAbsValue

  def getNamedNode(name: NamedNode): AAbsValue = {
    lazy val nt = AAbsValueNull
    name match {
      case DocumentNode => htmlDoc.getOrElse(nt)
      case DocumentElementNode => htmlHTML.getOrElse(nt)
      case HeadNode => htmlHead.getOrElse(nt)
      case TitleNode => htmlTitle.getOrElse(nt)
      case BodyNode => htmlBody.getOrElse(nt)
    }
  }

  var uid: Int = 0
  def uidGen(name: String): String = {
    val v = uid
    val id = s"#_$name.@$v"
    uid = uid + 1
    id
  }

  private def nodeClass(node: Node): String = {
    node match {
      case _: Attr => "Attr"
      case _: CDATASection => "CDATASection"
      case _: Comment => "Comment"
      case _: DocumentFragment => "DocumentFragment"
      case _: DocumentType => "DocumentType"
      case _: Text => "Text"
      case _: HTMLAnchorElement => "HTMLAnchorElement"
      case _: HTMLAppletElement => "HTMLAppletElement"
      case _: HTMLAreaElement => "HTMLAreaElement"
      case _: HTMLBaseElement => "HTMLBaseElement"
      case _: HTMLBaseFontElement => "HTMLBaseFontElement"
      case _: HTMLBodyElement => "HTMLBodyElement"
      case _: HTMLBRElement => "HTMLBRElement"
      case _: HTMLButtonElement => "HTMLButtonElement"
      case _: HTMLCollection => "HTMLCollection"
      case _: HTMLDirectoryElement => "HTMLDirectoryElement"
      case _: HTMLDivElement => "HTMLDivElement"
      case _: HTMLDListElement => "HTMLDListElement"
      case _: HTMLDocument => "HTMLDocument"
      case _: HTMLDOMImplementation => "HTMLDOMImplementation"
      case _: HTMLFieldSetElement => "HTMLFieldSetElement"
      case _: HTMLFontElement => "HTMLFontElement"
      case _: HTMLFormElement => "HTMLFormElement"
      case _: HTMLFrameElement => "HTMLFrameElement"
      case _: HTMLFrameSetElement => "HTMLFrameSetElement"
      case _: HTMLHeadElement => "HTMLHeadElement"
      case _: HTMLHeadingElement => "HTMLHeadingElement"
      case _: HTMLHRElement => "HTMLHRElement"
      case _: HTMLHtmlElement => "HTMLHtmlElement"
      case _: HTMLIFrameElement => "HTMLIFrameElement"
      case _: HTMLImageElement => "HTMLImageElement"
      case _: HTMLInputElement => "HTMLInputElement"
      case _: HTMLIsIndexElement => "HTMLIsIndexElement"
      case _: HTMLLabelElement => "HTMLLabelElement"
      case _: HTMLLegendElement => "HTMLLegendElement"
      case _: HTMLLIElement => "HTMLLIElement"
      case _: HTMLLinkElement => "HTMLLinkElement"
      case _: HTMLMapElement => "HTMLMapElement"
      case _: HTMLMenuElement => "HTMLMenuElement"
      case _: HTMLMetaElement => "HTMLMetaElement"
      case _: HTMLModElement => "HTMLModElement"
      case _: HTMLObjectElement => "HTMLObjectElement"
      case _: HTMLOListElement => "HTMLOListElement"
      case _: HTMLOptGroupElement => "HTMLOptGroupElement"
      case _: HTMLOptionElement => "HTMLOptionElement"
      case _: HTMLParagraphElement => "HTMLParagraphElement"
      case _: HTMLParamElement => "HTMLParamElement"
      case _: HTMLPreElement => "HTMLPreElement"
      case _: HTMLQuoteElement => "HTMLQuoteElement"
      case _: HTMLScriptElement => "HTMLScriptElement"
      case _: HTMLSelectElement => "HTMLSelectElement"
      case _: HTMLStyleElement => "HTMLStyleElement"
      case _: HTMLTableCaptionElement => "HTMLTableCaptionElement"
      case _: HTMLTableCellElement => "HTMLTableCellElement"
      case _: HTMLTableColElement => "HTMLTableColElement"
      case _: HTMLTableElement => "HTMLTableElement"
      case _: HTMLTableRowElement => "HTMLTableRowElement"
      case _: HTMLTableSectionElement => "HTMLTableSectionElement"
      case _: HTMLTextAreaElement => "HTMLTextAreaElement"
      case _: HTMLTitleElement => "HTMLTitleElement"
      case _: HTMLUListElement => "HTMLUListElement"
      case _: HTMLElement => "HTMLElement"
      case _ =>
        System.err.println(s"* Warning: an unknown element: ${node.getNodeName} - ${node.getClass.toString}")
        "HTMLUnknownElement"
    }
  }

  val mapID: mutable.AnyRefMap[Node, String] = mutable.AnyRefMap[Node, String]()

  def getID(node: Node, name: String = null, dynamic: Boolean = false): String = {
    if (node == null) null
    else {
      if (name != null) mapID.getOrElseUpdate(node, name)
      else {
        lazy val v =
          if (dynamic) node |> nodeClass
          else node |> nodeClass |> uidGen
        mapID.getOrElseUpdate(node, v)
      }
    }
  }

  def warning(message: String): SemanticsFun = {
    genAPI(si => {
      System.err.println(s"* Warning: $message")
      returnUndef(si)
    })
  }

  case class T(dom: Parser.T)

  def parseHTMLFromFile(name: String): T = {
    T(Parser.parseHTMLFromFile(name))
  }
  def toDOM(t: T): Document = Parser.toDOM(t.dom)

  def loadModel: Host = {
    Host.load(path, name)
  }

  def getProgram(t: T): List[CodeFragment] = {
    // Extract JavaScript code from HTML. Not event handlers.
    Parser.extractScript(t.dom)
  }

  def isSupported: String => Boolean = Parser.isSupported

  def joinObj(v1: AAbsObj, v2: AAbsObj): AAbsObj
  def joinValue(v1: AAbsValue, v2: AAbsValue): AAbsValue
  def joinIValue(v1: AAbsIValue, v2: AAbsIValue): AAbsIValue
  def joinState(v1: AAbsState, v2: AAbsState): AAbsState
  def joinLocSet(v1: AAbsLoc, v2: AAbsLoc): AAbsLoc
  def joinBool(v1: AAbsBool, v2: AAbsBool): AAbsBool

  def createElement(name: String, v_doc: AAbsValue)(s: SFInput): (AAbsValue, AAbsState) = {
    val tagName = name.toLowerCase
    val addr = s.newRecentAlloc(1)
    val a_style = s.newRecentAlloc(2)
    val a_childNodes = s.newRecentAlloc(3)
    val a_attr = s.newRecentAlloc(4)

    // TODO
    val i = -1 // addr.toLong
    val st = s.state

    val s_0 = st |> oldify(addr) |> oldify(a_style) |> oldify(a_childNodes) |> oldify(a_attr)
    val s_1 = doldify(i)(s_0)

    val l = addr
    val l_style = a_style
    val l_childNodes = a_childNodes
    val l_attr = a_attr
    val dl = DLoc.recent(i)

    mustNew(s_1, l)
    mustNew(s_1, l_style)
    mustNew(s_1, l_childNodes)
    mustNew(s_1, l_attr)
    //        assert(!s_1.ddomIn(dl))

    val node = owner.createElement(tagName)
    val cls = nodeClass(node)
    val l_proto = sysLoc(s"#$cls.prototype")
    // links between DOM node and JS obj.
    val o =
      newObject(l_proto, cls, bExt = BoolTrue) |>
        updatei(ii("Node"), toIValue(dl)) |>
        updatei(ii("Style"), toIValue(LtoValue(l_style))) |>
        updatei(ii("ChildNodes"), toIValue(LtoValue(l_childNodes))) |>
        updatei(ii("Attributes"), toIValue(LtoValue(l_attr)))

    val d = newDObject |> dupdate("obj", LtoValue(l))
    val (o_2, d_2) = updateProps(cls, node, v_doc)(o, d)
    val o_style = updatei(ii("Style"), toIValue(LtoValue(l_style)))(o_style_default)

    val o_n = newArrayObject(toAbsNum(0))

    // TODO children
    val s_2 = s_1 |>
      supdate(l_style, o_style) |>
      supdate(l, o_2) |>
      supdate(l_childNodes, emptyNodeList) |>
      supdate(l_attr, o_n)
    val s_3 = dupdate(dl, d_2)(s_2)

    (LtoValue(l), s_3)
  }

  def createDocumentFragment()(s: AAbsState, alloc: Long => ALoc, dalloc: Long => DLoc.T): (ALoc, AAbsState) = {
    val l = alloc(10)
    val l_childNodes = alloc(11)
    val dl = dalloc(10)
    val s_0 = s |> oldify(l) |> oldify(l_childNodes)
    val s_1 = s_0 |> doldify(dl.allocSite)

    mustNew(s_1, l)
    mustNew(s_1, l_childNodes)
    val node = owner.createDocumentFragment()
    val cls = nodeClass(node)
    val l_proto = sysLoc(s"#$cls.prototype")
    // links between DOM node and JS obj.
    val o =
      newObject(l_proto, cls, bExt = BoolTrue) |>
        updatei(ii("Node"), toIValue(dl)) |>
        updatei(ii("ChildNodes"), l_childNodes |> LtoValue |> toIValue)
    val d = newDObject |> dupdate("obj", l |> LtoValue)
    val (o_2, d_2) = updateProps(cls, node, AAbsValueNull)(o, d)

    val s_2 = s_1 |> supdate(l, o_2) |> supdate(l_childNodes, emptyNodeList) |> dupdate(dl, d_2)

    (l, s_2)
  }

  def updateDocument(doc: Node, s: AAbsState): AAbsState = {
    owner = doc.asInstanceOf[Document]
    val id = getID(doc)
    val l_childNodes = sysLoc(id + "_childNodes")
    val l = sysLoc(id)
    val dl = DLoc.sys(id)

    val o = s |> slookup(l) |> updatei(ii("Node"), toIValue(dl)) |>
      updatei(ii("ChildNodes"), toIValue(LtoValue(l_childNodes)))
    val d = newDocument |> dupdate("obj", LtoValue(l))

    // TODO initialize attributes.
    val (o_2, d_2) = updateProps(nodeClass(doc), doc, AAbsValueNull)(o, d)
    // TODO event handlers

    s |>
      supdate(l, o_2) |>
      supdate(l_childNodes, emptyNodeList) |>
      dupdate(dl, d_2)
  }

  def mustNew(s: AAbsState, l: ALoc): Unit = {
    assert(!(s |> sdomIn(l)), s"$l already exist:\n") // ${DomainPrinter.printObj(2, s(l))}")
  }

  def createElementA(node: Node, v_doc: AAbsValue, dynamic: Boolean = false, span: Option[(String, Int, Int)] = None)(s: AAbsState, alloc: String => ALoc, dalloc: String => DLoc.T, parseFunction: FuncParser): (ALoc, AAbsState) = {
    val id = getID(node, dynamic = dynamic)
    val l = alloc(id)
    val dl = dalloc(id)
    val l_style = alloc(id + "_style")
    val l_childNodes = alloc(id + "_childNodes")
    val l_attr = alloc(id + "_attr")

    val s_0 = s |> oldify(l) |> oldify(l_style) |> oldify(l_childNodes) |> oldify(l_attr)
    val s_1 = s_0 |> doldify(dl.allocSite)

    mustNew(s_1, l)
    mustNew(s_1, l_style)
    mustNew(s_1, l_childNodes)
    mustNew(s_1, l_attr)

    val cls = nodeClass(node)
    val l_proto = sysLoc(s"#$cls.prototype")

    val o =
      newObject(l_proto, cls) |>
        updatei(ii("Node"), toIValue(dl)) |>
        updatei(ii("Style"), toIValue(LtoValue(l_style))) |>
        updatei(ii("ChildNodes"), toIValue(LtoValue(l_childNodes))) |>
        updatei(ii("Attributes"), toIValue(LtoValue(l_attr)))

    val o_n =
      newArrayObject(toAbsNum(0))

    val d = newDObject |> dupdate("obj", LtoValue(l))
    val (o_2, d_2) = updateProps(cls, node, v_doc)(o, d)

    // event handlers
    val attrs = node.getAttributes
    val list =
      (List.empty[(String, ALoc, AAbsObj)] /: (0 until attrs.getLength))((d_i, i) => {
        val a = attrs.item(i).asInstanceOf[Attr]
        val fn = a.getName

        if (isEventHandler(fn)) {
          val name = fn.substring(2)
          val body = a.getValue
          val o = parseFunction(s"function (event) { $body }", span)
          val l_cb = alloc(s"${id}_$name")
          (name, l_cb, o) :: d_i
        } else if (fn.startsWith("on")) {
          System.err.println(s"* Warning: Suspicious attribute name: $fn")
          d_i
        } else d_i
      })
    val s_2 = (s_1 /: list)((s_i, nlo) => {
      mustNew(s_i, nlo._2)
      s_i |> supdate(nlo._2, nlo._3)
    })
    val d_3 = (d_2 /: list)((d_i, nlo) => d_i |> addEventListener(nlo._1, nlo._2, bubble = false))

    val o_style = o_style_default |> updatei(ii("Style"), toIValue(LtoValue(l_style)))
    val style = attr(node)("style")

    val o_style_2 =
      if (style == null || style.equals("")) o_style
      else o_style
    // TODO
    //      {
    //        val css = parseInlineCSS(style)
    //        (o_style /: (0 until css.getLength)) ((o_i, i) => {
    //          val key = css.item(i)
    //          val value = css.getPropertyValue(key)
    //
    //          cssTocssom.get(key) match {
    //            case Some(nkey) =>
    //              // TODO key and nkey must be binded. We can support this feature by getter/setter.
    //              if (nkey.equalsIgnoreCase(key))
    //                o_i |> oupdate(key, value)
    //              else
    //                o_i |> oupdate(key, value) |> oupdate(nkey, value)
    //            case _ =>
    //              System.err.println(s"* Warning: Ignored style sheet property: $key")
    //              o_i
    //          }
    //        })
    //      }

    // TODO children
    val s_3 = s_2 |> supdate(l_style, o_style_2) |>
      supdate(l_childNodes, emptyNodeList) |>
      supdate(l, o_2) |>
      supdate(l_attr, o_n)

    val s_4 = s_3 |> dupdate(dl, d_3)

    (l, s_4)
  }

  def createNode(node: Node, v_doc: AAbsValue, dynamic: Boolean = false, span: Option[(String, Int, Int)] = None)(s: AAbsState, alloc: String => ALoc, dalloc: String => DLoc.T, parseFunction: FuncParser): (ALoc, AAbsState) = {
    if (node.isInstanceOf[Element]) createElementA(node, v_doc, dynamic, span)(s, alloc, dalloc, parseFunction)
    else {
      val id = getID(node, dynamic = dynamic)

      val l = alloc(id)
      val dl = dalloc(id)
      val l_childNodes = alloc(id + "_childNodes")

      val s_0 = s |> oldify(l) |> oldify(l_childNodes)
      val s_1 = s_0 |> doldify(dl.allocSite)

      mustNew(s_1, l)
      mustNew(s_1, l_childNodes)

      val cls = nodeClass(node)
      val l_proto = sysLoc(s"#$cls.prototype")
      val o =
        newObject(l_proto, cls) |>
          updatei(ii("Node"), toIValue(dl)) |>
          updatei(ii("ChildNodes"), toIValue(LtoValue(l_childNodes)))
      val d = newDObject |> dupdate("obj", LtoValue(l))
      val (o_2, d_2) = updateProps(cls, node, v_doc)(o, d)

      // TODO event handlers
      // TODO children
      val s_2 = s_1 |> supdate(l, o_2) |> supdate(l_childNodes, emptyNodeList)
      val s_3 = s_2 |> dupdate(dl, d_2)

      (l, s_3)
    }
  }

  def lastSiblings(inode: DLoc.T)(s: AAbsState): DLocSet.T = {
    var visited = DLocSet.bottom

    def collects(icurr: DLoc.T, lasts: DLocSet.T): DLocSet.T = {
      if (visited.contains(icurr)) lasts
      else {
        visited = visited + icurr
        val nexts: DLocSet.T = nextSiblings(icurr)(s)
        (lasts /: nexts)((lasts_i, next) => {
          if (next == DLoc.nullv) lasts_i + icurr
          else collects(next, lasts_i)
        })
      }
    }

    collects(inode, DLocSet.bottom)
  }

  def firstChilds(inode: DLoc.T)(s: AAbsState): DLocSet.T = s |> dlookup(inode) |> ddlookup("firstChild") |> dlocset

  def nextSiblings(inode: DLoc.T)(s: AAbsState): DLocSet.T = s |> dlookup(inode) |> ddlookup("nextSibling") |> dlocset

  def parentNodes(inode: DLoc.T)(s: AAbsState): DLocSet.T = s |> dlookup(inode) |> ddlookup("parentNode") |> dlocset

  private def insertFirst(iparent: DLoc.T, inode: DLoc.T)(s: AAbsState): AAbsState = {
    val dn = s |> dlookup(inode) |> dupdate("parentNode", iparent |> DtoValue)
    val dp = s |> dlookup(iparent)
    val dp_2 = dp |> dupdate("firstChild", inode |> DtoValue)
    val firsts = dp.lookup("firstChild").dlocset
    val dn_2 = dn |> dupdate("nextSibling", firsts |> DStoValue)
    s |> dupdate(iparent, dp_2) |> dupdate(inode, dn_2)
  }

  private def insert(iparent: DLoc.T, pos: DLoc.T, inode: DLoc.T)(s: AAbsState): AAbsState = {
    val dp = s |> dlookup(pos)
    val nexts = dp.lookup("nextSibling").dlocset
    val dn = s |> dlookup(inode) |> dupdate("nextSibling", nexts |> DStoValue) |> dupdate("parentNode", iparent |> DtoValue)
    val dp_2 = dp |> dupdate("nextSibling", inode |> DtoValue)

    s |> dupdate(inode, dn) |> dupdate(pos, dp)
  }

  def lastChild(l_node: ALoc)(s: AAbsState): AAbsValue = {
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset

    val ifirsts = (DLocSet.bottom /: inodes)((d_i, inode) => d_i + firstChilds(inode)(s))

    val (first, restCases) = ifirsts.partition(l => l == DLoc.nullv)

    def find(inode: DLoc.T): Boolean = {
      if (inode == DLoc.nullv) false
      else {
        val dn = s |> dlookup(inode)
        dn.lookup("nextSibling").dlocset <> (DLocSet.bottom + DLoc.nullv) </ DLocSet.bottom
      }
    }

    val iposs = (DLocSet.bottom /: restCases)((dlset_i, rest) => {
      dlset_i + collectSiblings(rest, find)(s)
    })

    val lset = (ValueBot /: iposs)((lset_i, ipos) => joinValue(lset_i, s |> dlookup(ipos) |> ddlookup("obj")))
    val v_first =
      if (first.nonEmpty) AAbsValueNull
      else ValueBot
    val v_last = lset

    joinValue(v_first, v_last)
  }

  // TODO Need to consider exception cases.
  def insertBefore(l_parent: ALoc, l_node: ALoc, l_before: ALoc)(s: AAbsState): AAbsState = {
    val iparents = s |> slookup(l_parent) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val ibefores = s |> slookup(l_before) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset

    val ifirsts = (DLocSet.bottom /: iparents)((d_i, iparent) => d_i + firstChilds(iparent)(s))

    def find(inode: DLoc.T): Boolean = {
      if (inode == DLoc.nullv) false
      else {
        val dn = s |> dlookup(inode)
        (dn.lookup("nextSibling").dlocset <> ibefores) </ DLocSet.bottom &&
          (dn.lookup("parentNode").dlocset <> iparents) </ DLocSet.bottom
      }
    }

    val (firstCases, restCases) = ifirsts.partition((inode: DLoc.T) => {
      if (inode == DLoc.nullv) true
      else {
        val dn = s |> dlookup(inode)
        dn.lookup("parentNode").dlocset <> iparents </ DLocSet.bottom && ibefores.contains(inode)
      }
    })

    val iposs = (DLocSet.bottom /: restCases)((dlset_i, rest) => {
      dlset_i + collectSiblings(rest, find)(s)
    })
    (StateBot /: iparents)((s_i, iparent) => {
      (s_i /: inodes)((s_i2, inode) => {
        val s_i3 =
          if (!firstCases.isEmpty)
            joinState(s_i2, insertFirst(iparent, inode)(s))
          else s_i2
        (s_i3 /: iposs)((s_i4, ipos) => joinState(s_i4, insert(iparent, ipos, inode)(s)))
      })
    })
  }

  def appendChildA(l_parent: ALoc, l_node: ALoc)(s: AAbsState): AAbsState = {
    val iparents = s |> slookup(l_parent) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    //      System.out.println(s"* Info: appendChild2 - ${iparents.size * inodes.size}")
    (StateBot /: iparents)((s_i, iparent) => {
      (s_i /: inodes)((s_ii, inode) => {
        joinState(s_i, appendChildi(iparent, inode)(s))
      })
    })
  }

  private def removeFirstChild(iparent: DLoc.T)(s: AAbsState): AAbsState = {
    val dp = s |> dlookup(iparent)
    val firsts = dp.lookup("firstChild").dlocset
    val firsts_n = firsts.nonNulls

    val s_remove =
      if (firsts_n.nonEmpty) {
        val nexts = (DLocSet.bottom /: firsts_n)((d_i, ifirst) => d_i + nextSiblings(ifirst)(s))
        val dp_2 = dp |> dupdate("firstChild", nexts |> DStoValue)
        s |> dupdate(iparent, dp_2)
      } else {
        StateBot
      }

    val s_nothing =
      if (firsts.hasNull) s |> dupdate(iparent, dp |> dupdate("firstChild", DLoc.nullv |> DtoValue))
      else StateBot

    joinState(s_remove, s_nothing)
  }

  private def removeNextSibling(iparent: DLoc.T, pos: DLoc.T)(s: AAbsState): AAbsState = {
    val dp = s |> dlookup(pos)
    val nexts = dp.lookup("nextSibling").dlocset
    val nextnexts = (DLocSet.bottom /: nexts.nonNulls)((d_i, inext) => d_i + nextSiblings(inext)(s))
    val nextnull = if (nexts.hasNull) DLocSet.bottom + DLoc.nullv else DLocSet.bottom
    val dp_2 = dp |> dupdate("nextSibling", (nextnexts + nextnull) |> DStoValue)
    s |> dupdate(pos, dp_2)
  }

  def removeChild(l_parent: ALoc, l_node: ALoc)(s: AAbsState): AAbsState = {
    val iparents = s |> slookup(l_parent) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset

    val ifirsts = (DLocSet.bottom /: iparents)((d_i, iparent) => d_i + firstChilds(iparent)(s))

    def find(inode: DLoc.T): Boolean = {
      if (inode == DLoc.nullv) false
      else {
        val dn = s |> dlookup(inode)
        dn.lookup("nextSibling").dlocset <> inodes </ DLocSet.bottom &&
          dn.lookup("parentNode").dlocset <> iparents </ DLocSet.bottom
      }
    }

    val (firstCases, restCases) = ifirsts.partition((inode: DLoc.T) => {
      if (inode == DLoc.nullv) false
      else {
        val dn = s |> dlookup(inode)
        dn.lookup("parentNode").dlocset <> iparents </ DLocSet.bottom && inodes.contains(inode)
      }
    })

    val iposs = (DLocSet.bottom /: restCases)((dlset_i, rest) => {
      dlset_i + collectSiblings(rest, find)(s)
    })
    (StateBot /: iparents)((s_i, iparent) => {
      val s_i2 =
        if (!firstCases.isEmpty)
          joinState(s_i, removeFirstChild(iparent)(s))
        else s_i
      (s_i2 /: iposs)((s_i3, ipos) => joinState(s_i3, removeNextSibling(iparent, ipos)(s)))
    })
  }

  private def appendChildi(iparent: DLoc.T, inode: DLoc.T)(s: AAbsState): AAbsState = {
    val lset_parent = s |> dlookup(iparent) |> ddlookup("obj")
    val firsts = firstChilds(iparent)(s)

    val firsts_n = firsts.hasNull
    val ilasts = (DLocSet.bottom /: firsts.nonNulls)((locs_i, first) => locs_i + lastSiblings(first)(s))
    val node = s |> dlookup(inode) |> dupdate("parentNode", DtoValue(iparent)) |> dupdate("nextSibling", DtoValue(DLoc.nullv))

    val s_last =
      (StateBot /: ilasts)((s_i, ilast) => {
        val last = s |> dlookup(ilast)
        val nlast = last |> dupdate("nextSibling", DtoValue(inode))
        joinState(s_i, s |> dupdate(ilast, nlast) |> dupdate(inode, node))
      })

    val s_first =
      if (firsts_n) {
        val o = s |> dlookup(iparent)
        s |> dupdate(iparent, o |> dupdate("firstChild", DtoValue(inode))) |> dupdate(inode, node)
      } else StateBot

    val s_n = joinState(s_first, s_last)

    val lset_childNodes = lsetFoldLeft(lset_parent)(ValueBot)((lset_i, l) => {
      joinValue(lset_i, s |> slookup(l) |> lookupi(ii("ChildNodes")) |> ItoValue)
    })
    val lset_nodes = s |> dlookup(inode) |> ddlookup("obj")
    lsetFoldLeft(lset_childNodes)(StateBot)((s_i, l) => joinState(s_i, appendLast(l, lset_nodes)(s_n)))
  }

  private def appendLast(l_childNodes: ALoc, lset_nodes: AAbsValue)(s: AAbsState): AAbsState = {
    val o = s |> slookup(l_childNodes)
    val length = o |> lookupi(ii("Length"))
    length |> ItoValue |> NgetSingle match {
      case Some(i) =>
        val n_length = i + 1
        val o_n =
          o |> updatei(ii("Length"), toIValue(NtoValue(toAbsNum(n_length)))) |>
            update(i.toInt.toString, toDataProp(lset_nodes))
        s |> supdate(l_childNodes, o_n)
      case _ =>
        val v_len = UInt
        val lset = o |> lookup(IntStr)
        val lset_n = joinValue(lset, lset_nodes)
        val o_n =
          o |> updatei(ii("Length"), toIValue(NtoValue(v_len))) |>
            update(IntStr, toDataProp(lset_n))
        s |> supdate(l_childNodes, o_n)
    }
  }

  def lsetFoldLeft[T](v: AAbsValue)(i: T)(f: (T, ALoc) => T): T
  def lsetFoldLeftL[T](v: AAbsLoc)(i: T)(f: (T, ALoc) => T): T

  def appendChild(parent: Node, node: Node)(s: AAbsState): AAbsState = {
    val iparent = DLoc.sys(getID(parent))
    val inode = DLoc.sys(getID(node))
    appendChildi(iparent, inode)(s)
  }

  def genInitialDOMTree(node: Document, getSpan: Node => Option[(String, Int, Int)], s: AAbsState, parseFunction: FuncParser): AAbsState = {
    def toValue(s: String): Option[AAbsValue] = Some(LtoValue(sysLoc(s)))

    val global = GlobalLoc
    val dglobal = DLoc.sys("#Global")
    val s_1 =
      s |>
        supdate(global, slookup(global)(s) |> updatei(ii("Node"), toIValue(dglobal))) |>
        dupdate(dglobal, newDObject |> dupdate("obj", LtoValue(global)))

    val documentID: String = getID(node, docName)
    htmlDoc = toValue(documentID)
    val s_2 = updateDocument(node, s_1)
    val v_doc = LtoValue(sysLoc(getID(node)))

    def tree(node: Node, parent: Node)(s_i: AAbsState): AAbsState = {
      if (node == null) s_i
      else {
        nodeClass(node) match {
          case "HTMLHtmlElement" if htmlHTML.isEmpty => htmlHTML = toValue(getID(node, docHTMLName))
          case "HTMLHeadElement" if htmlHead.isEmpty => htmlHead = toValue(getID(node, docHeadName))
          case "HTMLTitleElement" if htmlTitle.isEmpty => htmlTitle = toValue(getID(node, docTitleName))
          case "HTMLBodyElement" if htmlBody.isEmpty => htmlBody = toValue(getID(node, docBodyName))
          case _ => // do nothing
        }

        def getAddr(id: String): (ALoc, DLoc.T) = {
          val l = sysLoc(id)
          val dl = DLoc.sys(id)
          (l, dl)
        }

        def Alloc(id: String): ALoc = sysLoc(id)

        def DAlloc(id: String): DLoc.T = DLoc.sys(id)

        createNode(node, v_doc, span = getSpan(node))(s_i, Alloc, DAlloc, parseFunction)._2 |>
          appendChild(parent, node) |>
          tree(node.getFirstChild, node) |>
          tree(node.getNextSibling, parent)
      }
    }

    tree(node.getFirstChild, node)(s_2)
  }

  private[this] def oupdate(key: String, value: String)(o: AAbsObj): AAbsObj =
    o |> update(key, toDataProp(StoValue(toAbsStr(value))))

  private[this] def uupdate(key: String)(o: AAbsObj): AAbsObj =
    o |> update(key, toDataProp(StoValue(OtherStr)))

  lazy val emptyNodeList: AAbsObj = {
    val l_proto = sysLoc(s"#NodeList.prototype")
    newObject(l_proto, "NodeList") |> updatei(ii("Length"), toIValue(NtoValue(toAbsNum(0))))
  }

  lazy val o_style_default: AAbsObj = {
    val l_style_proto = sysLoc(s"#CSSStyleDeclaration.prototype")
    val o_style = newObject(l_style_proto, "CSSStyleDeclaration")

    (o_style /: cssTocssom)((o_style_i, kv) => o_style_i |> oupdate(kv._1, "") |> oupdate(kv._2, ""))
  }
  lazy val o_style_computed_default: AAbsObj = {
    val l_style_proto = sysLoc(s"#CSSStyleDeclaration.prototype")
    val o_style = newObject(l_style_proto, "CSSStyleDeclaration")

    (o_style /: cssTocssom)((o_style_i, kv) => o_style_i |> uupdate(kv._1) |> uupdate(kv._2))
  }

  def attrs(node: Node): HashMap[String, String] = {
    val map = node.getAttributes
    val length = map.getLength
    (HashMap.empty[String, String] /: (0 until length)) {
      case (m_i, i) =>
        val ith = map.item(i)
        val name = ith.getNodeName
        val value = ith.getNodeValue
        if (isEventHandler(name)) m_i
        else m_i + (name -> value)
    }
  }

  def attr(node: Node)(prop: String): String = {
    val map = node.getAttributes
    if (map == null) null
    else {
      val n = map.getNamedItem(prop)
      val attr = n.asInstanceOf[Attr]
      if (attr == null) ""
      else attr.getValue
    }
  }

  def updateIProp(name: String, value: CValue)(od: (AAbsObj, DNode.T)): (AAbsObj, DNode.T)
  def updateIProp(name: String, value: AAbsValue)(od: (AAbsObj, DNode.T)): (AAbsObj, DNode.T) =
    (od._1, od._2 |> dupdate(name, value))
  def updateProp(name: String, value: CValue)(od: (AAbsObj, DNode.T)): (AAbsObj, DNode.T)
  def updateProp(name: String, value: AAbsValue)(od: (AAbsObj, DNode.T)): (AAbsObj, DNode.T) = {
    (od._1 |> update(name, toDataProp(value, w = BoolTrue, e = BoolFalse, c = BoolTrue)), od._2)
  }

  def updateProps(name: String, node: Node, v_doc: AAbsValue)(o: AAbsObj, d: DNode.T): (AAbsObj, DNode.T)

  def StrictEquals(v: AAbsStr)(v2: AAbsStr): AAbsBool
  def isARelated(s: AAbsStr)(v: AAbsStr): Boolean
  def isRelated(s: String)(v: AAbsStr): Boolean
  def strval(v: AAbsValue): AAbsStr
  def pvalue(v: AAbsValue): AAbsPValue
  def dlocset(v: AAbsValue): DLocSet.T
  def locset(v: AAbsValue): AAbsLoc
  def ddlookup(s: String)(o: DNode.T): AAbsValue
  def ddlookupD(s: String, v: AAbsValue)(o: DNode.T): AAbsValue
  def dlookup(l: DLoc.T)(s: AAbsState): DNode.T
  def lookup(s: AAbsStr)(o: AAbsObj): AAbsValue
  def lookupi(n: AIName)(o: AAbsObj): AAbsIValue
  def dupdate(name: String, v: AAbsValue)(o: DNode.T): DNode.T
  def updatei(name: AIName, v: AAbsIValue)(o: AAbsObj): AAbsObj
  def update(name: String, v: AAbsDataProp)(o: AAbsObj): AAbsObj
  def update(name: AAbsStr, v: AAbsDataProp)(o: AAbsObj): AAbsObj
  def toAbsStr(s: String): AAbsStr
  def newObject(l_proto: ALoc, cls: String, bExt: AAbsBool = BoolTrue): AAbsObj
  def newArrayObject(v: AAbsNum): AAbsObj
  def sysLoc(name: String): ALoc
  def gamma_str(s: AAbsStr): Option[Set[String]]
  def filterCallable(s: AAbsState, ls: AAbsValue): AAbsValue
  def isNonEmpty(v: AAbsValue): Boolean
  def isNonZero(v: AAbsNum): Boolean
  def newAddr(): ALoc
  def callToString(s: SFInput)(v: AAbsValue): Set[AAbsStr]
  def oldify(l: ALoc)(s: AAbsState): AAbsState
  def doldify(l: Long)(s: AAbsState): AAbsState
  def isSingleton(l: ALoc): Boolean
  def isSingleStr(s: AAbsStr): Boolean

  def toBoolean(v: AAbsValue): AAbsBool

  def toIValue(dl: DLoc.T): AAbsIValue
  def toIValue(l: AAbsValue): AAbsIValue

  def DtoValue(dl: DLoc.T): AAbsValue
  def DStoValue(dl: DLocSet.T): AAbsValue
  def LtoValue(l: ALoc): AAbsValue
  def LStoValue(l: AAbsLoc): AAbsValue
  def NtoValue(n: AAbsNum): AAbsValue
  def StoValue(s: AAbsStr): AAbsValue
  def ItoValue(i: AAbsIValue): AAbsValue
  def BtoValue(i: AAbsBool): AAbsValue
  def NgetSingle(v: AAbsValue): Option[Double]
  def remove(l: ALoc)(v: AAbsValue): AAbsValue

  def sdomIn(l: ALoc)(s: AAbsState): Boolean
  def domIni(l: AIName)(o: AAbsObj): AAbsBool
  def slookup(l: ALoc)(s: AAbsState): AAbsObj
  def dupdate(dl: DLoc.T, od: DNode.T)(s: AAbsState): AAbsState
  def supdate(l: ALoc, o: AAbsObj)(s: AAbsState): AAbsState
  def toAbsNum(i: Double): AAbsNum
  def toDataProp(v: AAbsValue): AAbsDataProp
  def toDataProp(v: AAbsValue, w: AAbsBool, e: AAbsBool, c: AAbsBool): AAbsDataProp
  def isSingleLoc(v: AAbsValue): Boolean
  def getSingleLoc(v: AAbsValue): ALoc
  def filter(v: AAbsValue): (AAbsValue, AAbsValue)
  def toPBot(v: AAbsValue): AAbsValue
  def isBottom(v: AAbsValue): Boolean
  def isPBottom(v: AAbsPValue): Boolean
  def isBBottom(v: AAbsBool): Boolean
  def hasNull(v: AAbsValue): Boolean

  val BoolTrue: AAbsBool
  val BoolFalse: AAbsBool
  val BoolTop: AAbsBool
  val BoolBot: AAbsBool
  val StrTop: AAbsStr
  val OtherStr: AAbsStr
  val IntStr: AAbsStr
  val UInt: AAbsNum
  val GlobalLoc: ALoc
  val ValueBot: AAbsValue
  val StateBot: AAbsState
  val LocSetBot: AAbsLoc
  val AbsObjBot: AAbsObj
  val emptyObject: AAbsObj

  lazy val newDObject: DNode.T = {
    DNode.bottom |>
      dupdate("firstChild", DLoc.nullv |> DtoValue) |>
      dupdate("nextSibling", DLoc.nullv |> DtoValue) |>
      dupdate("parentNode", DLoc.nullv |> DtoValue)
  }
  lazy val newDocument: DNode.T = {
    DNode.bottom |>
      dupdate("firstChild", DLoc.nullv |> DtoValue) |>
      dupdate("nextSibling", DLoc.nullv |> DtoValue) |>
      dupdate("parentNode", DLoc.nullv |> DtoValue)
  }

  def has(b: Boolean)(a: AAbsBool): Boolean
  def orderBool(b0: AAbsBool, b1: AAbsBool): Boolean
  def addEventListener(cname: String, h: ALoc, bubble: Boolean)(o: DNode.T): DNode.T
  def removeEventListener(cname: String, h: ALoc, bubble: Boolean)(o: DNode.T): DNode.T

  private def getDNodes(s: AAbsState, lset: AAbsLoc): (DLocSet.T, Boolean) = {
    lsetFoldLeftL(lset)((DLocSet.bottom, false)) {
      (ll_i, l) =>
        val o = s |> slookup(l)
        val d = o |> domIni(ii("Node"))
        val lset1 = if (orderBool(BoolTrue, d)) ll_i._1 + (o |> lookupi(ii("Node")) |> ItoValue |> dlocset) else ll_i._1
        val lset2 = if (orderBool(BoolFalse, d)) true else ll_i._2
        (lset1, lset2)
    }
  }

  def getDPropDefault(prop: String, locset: AAbsLoc, default: AAbsValue)(s: SFInput): (AAbsValue, Set[AException]) = {
    val (dlset_ok, b_notok) = getDNodes(s.state, locset)

    val pv = (ValueBot /: dlset_ok)((v_i, dl) => joinValue(v_i, s.state |> dlookup(dl) |> ddlookupD(prop, default)))
    val es = if (b_notok) AExceptionBot + AError else AExceptionBot

    (pv, es)
  }

  private def createHTMLCollection(lset: AAbsLoc): AAbsObj = {
    val cls = "HTMLCollection"
    val l_proto = sysLoc(s"#$cls.prototype")
    newObject(l_proto, cls, bExt = BoolTrue) |>
      updatei(ii("Length"), UInt |> NtoValue |> toIValue) |>
      update(IntStr, toDataProp(lset |> LStoValue, BoolTrue, BoolTrue, BoolTrue))
  }

  private def createHTMLCollection(v: Vector[AAbsLoc]): AAbsObj = {
    val cls = "HTMLCollection"
    val l_proto = sysLoc(s"#$cls.prototype")
    // links between DOM node and JS obj.
    val o =
      newObject(l_proto, cls, bExt = BoolTrue) |>
        updatei(ii("Length"), toIValue(NtoValue(toAbsNum(v.length))))

    (o /: v.indices)((o_i, i) => {
      val k = v(i)
      o_i |> update(toAbsStr(i.toString), toDataProp(k |> LStoValue, BoolTrue, BoolTrue, BoolTrue))
    })
  }

  def createTextNode(text: AAbsStr)(s: AAbsState, alloc: Long => ALoc, dalloc: Long => DLoc.T): (ALoc, AAbsState) = {
    // TODO oldify the locations.
    val l = alloc(10)
    val l_childNodes = alloc(11)
    val dl = dalloc(10)

    val s_0 = s |> oldify(l) |> oldify(l_childNodes)
    val s_1 = s_0 |> doldify(dl.allocSite)

    mustNew(s_1, l)
    mustNew(s_1, l_childNodes)

    val (o_2, d_2) =
      text |> gamma_str match {
        case Some(txts) =>
          ((AbsObjBot, DNode.bottom) /: txts)((od, text) => {
            val node = Parser.createTextNode(text)
            val cls = "Text"
            val l_proto = sysLoc(s"#$cls.prototype")
            val o =
              newObject(l_proto, cls) |>
                updatei(ii("Node"), dl |> DtoValue |> toIValue) |>
                updatei(ii("ChildNodes"), l_childNodes |> LtoValue |> toIValue)
            val d = newDObject |> dupdate("obj", l |> LtoValue)
            val (on, dn) = updateProps(cls, node, AAbsValueNull)(o, d)
            (joinObj(od._1, on), od._2 + dn)
          })
        case _ =>
          System.err.println("TODO: The created text node have a concrete value.")
          val node = Parser.createTextNode("abs text")
          val cls = "Text"
          val l_proto = sysLoc(s"#$cls.prototype")
          val o =
            newObject(l_proto, cls) |>
              updatei(ii("Node"), dl |> DtoValue |> toIValue) |>
              updatei(ii("ChildNodes"), l_childNodes |> LtoValue |> toIValue)
          val d = newDObject |> dupdate("obj", l |> LtoValue)
          updateProps(cls, node, AAbsValueNull)(o, d)
      }

    (l, s_1 |> supdate(l, o_2) |> supdate(l_childNodes, emptyNodeList) |> dupdate(dl, d_2))
  }

  def createComment(text: AAbsStr)(si: SFInputT): (ALoc, AAbsState) = {
    // TODO oldify the locations.
    val l = si.newRecentAlloc(10)
    val l_childNodes = si.newRecentAlloc(11)
    val dl = si.newDAlloc(10)
    val s = si.state
    mustNew(s, l)
    mustNew(s, l_childNodes)

    val (o_2, d_2) =
      text |> gamma_str match {
        case Some(txts) =>
          ((AbsObjBot, DNode.bottom) /: txts)((od, text) => {
            val node = Parser.createComment(text)
            val cls = "Text"
            val l_proto = sysLoc(s"#$cls.prototype")
            val o =
              newObject(l_proto, cls) |>
                updatei(ii("Node"), dl |> DtoValue |> toIValue) |>
                updatei(ii("ChildNodes"), l_childNodes |> LtoValue |> toIValue)
            val d = newDObject |> dupdate("obj", l |> LtoValue)
            val (on, dn) = updateProps(cls, node, AAbsValueNull)(o, d)
            (joinObj(od._1, on), od._2 + dn)
          })
        case _ =>
          System.err.println("TODO: The created comment node have a concrete value.")
          val node = Parser.createComment("abs text")
          val cls = "Text"
          val l_proto = sysLoc(s"#$cls.prototype")
          val o =
            newObject(l_proto, cls) |>
              updatei(ii("Node"), dl |> DtoValue |> toIValue) |>
              updatei(ii("ChildNodes"), l_childNodes |> LtoValue |> toIValue)
          val d = newDObject |> dupdate("obj", l |> LtoValue)
          updateProps(cls, node, AAbsValueNull)(o, d)
      }

    (l, s |> supdate(l, o_2) |> supdate(l_childNodes, emptyNodeList) |> dupdate(dl, d_2))
  }

  def getDProp(prop: String, locset: AAbsLoc)(s: SFInput): (AAbsValue, Set[AException]) = getDPropDefault(prop, locset, ValueBot)(s)

  def getDPropOfThis(prop: String)(s: SFInput): (AAbsValue, Set[AException]) = getDProp(prop, s.lset_this)(s)

  def getDPropOfThisD(prop: String, default: AAbsValue)(s: SFInput): (AAbsValue, Set[AException]) = getDPropDefault(prop, s.lset_this, default)(s)

  def forms_get(l_node: ALoc, l_r: ALoc)(s: AAbsState): AAbsState = {
    def dfs(l: DLoc.T, ith: Int, aa: Vector[AAbsLoc]): (Int, Vector[AAbsLoc]) = {
      if (l == DLoc.nullv) (ith, aa)
      else {
        val dn = s |> dlookup(l)
        val atag = dn |> ddlookup("localName")

        val aa_n: Vector[AAbsLoc] =
          if (isBottom(atag)) {
            // not an element
            aa
          } else {
            lazy val lset = dn |> ddlookup("obj")
            // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
            // TODO we ignore namespace

            lazy val tag = atag //._1
            lazy val b0 = hasNull(atag)

            // TODO Check the semantics. we have to split the following false case and join it to the result.
            lazy val b1 =
              atag |> strval |> gamma_str match {
                case Some(tags) =>
                  // TODO need to check
                  tags.exists(t => "form".equalsIgnoreCase(t))
                case _ =>
                  if (b0) false
                  else throw new ModelError("TODO")
              }

            if (atag |> pvalue |> isPBottom) {
              aa
            } else if (b1) {
              aa :+ (lset |> locset)
            } else {
              aa
            }
          }
        val firsts = firstChilds(l)(s)
        val nexts = nextSiblings(l)(s)
        assert(firsts.isSingleton)
        assert(nexts.isSingleton)
        val first = firsts.head
        val (nith, aa_2) = dfs(first, ith + 1, aa_n)

        val next = nexts.head
        val (nith2, aa_3) = dfs(next, nith + 1, aa_2)

        (nith2, aa_3)
      }
    }

    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))

    try {
      assert(fs.isSingleton)

      val first = fs.head
      val array = dfs(first, 0, Vector.empty[AAbsLoc])._2

      s |> supdate(l_r, createHTMLCollection(array))
    } catch {
      case _: AssertionError =>
        var visited: DLocSet.T = DLocSet.bottom

        def adfs(l: DLoc.T): AAbsValue = {
          if (l == DLoc.nullv) LocSetBot |> LStoValue
          else if (visited.contains(l)) LocSetBot |> LStoValue
          else {
            visited = visited + l

            val dn = s |> dlookup(l)
            val atag = dn |> ddlookup("localName")

            val aa_n: AAbsValue =
              if (atag |> isBottom) {
                // not an element
                LocSetBot |> LStoValue
              } else {
                lazy val lset = dn |> ddlookup("obj")
                // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
                // TODO we ignore namespace

                lazy val tag = atag
                lazy val b0 = atag |> hasNull

                // TODO Check the semantics. we have to split the following false case and join it to the result.
                lazy val b1 =
                  tag |> strval |> gamma_str match {
                    case Some(tags) =>
                      // TODO need to check
                      tags.exists(t => "form".equalsIgnoreCase(t))
                    case _ =>
                      if (b0) false
                      else throw new ModelError("TODO")
                  }

                if (atag |> pvalue |> isPBottom) {
                  ValueBot
                } else if (b1) {
                  lset
                } else {
                  ValueBot
                }
              }
            val firsts = firstChilds(l)(s)
            val nexts = nextSiblings(l)(s)
            val lset_1 = (ValueBot /: firsts)((lset_i, first) => joinValue(lset_i, adfs(first)))
            val lset_2 = (ValueBot /: nexts)((lset_i, next) => joinValue(lset_i, adfs(next)))
            joinValue(lset_1, joinValue(lset_2, aa_n))
          }
        }

        val lset = (ValueBot /: fs)((lset_i, f) => joinValue(lset_i, adfs(f)))
        s |> supdate(l_r, createHTMLCollection(lset |> locset))
    }
  }

  protected def dfs_general(fields: List[DLoc.T => AAbsState => DLocSet.T], selector: DLoc.T => Boolean, s: AAbsState, ifirsts: DLocSet.T): DLocSet.T = {
    var visited = DLocSet.bottom

    def dfs(d: DLoc.T): DLocSet.T = {
      if (d == DLoc.nullv) DLocSet.bottom
      else if (visited.contains(d)) DLocSet.bottom
      else {
        visited = visited + d
        val dlset =
          if (selector(d)) DLocSet.bottom + d
          else DLocSet.bottom

        val nexts =
          (DLocSet.bottom /: fields)((d_i, f) => {
            d_i + f(d)(s)
          })

        (dlset /: nexts)((dlset_i, next) => dlset_i + dfs(next))
      }
    }

    (DLocSet.bottom /: ifirsts)((d_i, dl) => d_i + dfs(dl))
  }

  def collectSiblings(ifirst: DLoc.T, selector: DLoc.T => Boolean)(s: AAbsState): DLocSet.T = {
    dfs_general(List(nextSiblings), selector, s, DLocSet.bottom + ifirst)
  }

  def collectAncestors(is: DLocSet.T, selector: DLoc.T => Boolean)(s: AAbsState): DLocSet.T = {
    dfs_general(List(parentNodes), selector, s, is)
  }

  def collectDecendants(ifirsts: DLocSet.T, selector: DLoc.T => Boolean)(s: AAbsState): DLocSet.T = {
    dfs_general(List(nextSiblings, firstChilds), selector, s, ifirsts)
  }

  def setInterval(handlers: AAbsValue, addr: ALoc)(s: AAbsState): (AAbsValue, AAbsState) = {
    val ctxs = getNamedNode(DocumentElementNode)
    val l_r = addr

    val s_0 = s |> oldify(l_r)
    val s_1 = s_0 |> supdate(l_r, emptyObject |> updatei(ii("Interval"), toIValue(handlers)))
    val s_n =
      lsetFoldLeft(ctxs)(StateBot)((s_i, l) => {
        val o = s_0 |> slookup(l)
        val old = o |> lookupi(ii("Interval"))
        val s_2 = s_1 |> supdate(l, o |> updatei(ii("Interval"), joinIValue(old, toIValue(LtoValue(l_r)))))
        joinState(s_i, s_2)
      })
    (LtoValue(l_r), s_n)
  }

  def clearInterval(l: ALoc)(s: AAbsState): AAbsState = {
    if (l |> isSingleton) {
      val o = s |> slookup(l)
      val v = (o |> lookupi(ii("Interval"))) |> ItoValue |> remove(l) |> toIValue
      s |> supdate(l, o |> updatei(ii("Interval"), v))
    } else s
  }

  def getComputedStyle(lset_elems: AAbsValue, pseudoElt: String, l_r: ALoc)(s: AAbsState): AAbsState = {
    assert(pseudoElt == null)
    val lset_style = lsetFoldLeft(lset_elems)(ValueBot)((lset_i, l) => {
      joinValue(lset_i, s |> slookup(l) |> lookupi(ii("Style")) |> ItoValue)
    })
    val o = o_style_computed_default |> updatei(ii("Style"), toIValue(lset_style))
    s |> oldify(l_r) |> supdate(l_r, o)
  }

  private[this] def getElementsByClassNameI(l_node: ALoc, clsName: String)(s: AAbsState): AAbsObj = {
    def dfs(l: DLoc.T, ith: Int, aa: Vector[AAbsLoc]): (Int, Vector[AAbsLoc]) = {
      if (l == DLoc.nullv) (ith, aa)
      else {
        val dn = s |> dlookup(l)
        val acls = dn |> ddlookup("className")

        val aa_n: Vector[AAbsLoc] =
          if (acls |> isBottom) {
            // not an element
            aa
          } else {
            lazy val lset = dn |> ddlookup("obj") |> locset //._2
            // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
            // TODO we ignore namespace

            lazy val cls = acls //._1
            lazy val b0 = hasNull(acls)

            // TODO Check the semantics. we have to split the following false case and join it to the result.
            lazy val b1 =
              cls |> strval |> gamma_str match {
                case Some(clses) =>
                  // TODO need to check
                  val clsess = clses.map(s => s.split(' '))
                  clses.exists(clsList => clsList.split(' ').exists(cls => clsName.equalsIgnoreCase(cls)))
                case _ =>
                  if (b0) false
                  else true // throw new InternalError("TODO")
              }

            if (cls |> pvalue |> isPBottom) {
              aa
            } else if (b1) {
              aa :+ lset
            } else {
              aa
            }
          }
        val firsts = firstChilds(l)(s)
        val nexts = nextSiblings(l)(s)
        assert(firsts.isSingleton)
        assert(nexts.isSingleton)
        val first = firsts.head
        val (nith, aa_2) = dfs(first, ith + 1, aa_n)

        val next = nexts.head
        val (nith2, aa_3) = dfs(next, nith + 1, aa_2)

        (nith2, aa_3)
      }
    }

    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))

    try {
      assert(fs.isSingleton)

      val first = fs.head
      val array = dfs(first, 0, Vector.empty[AAbsLoc])._2

      createHTMLCollection(array)
    } catch {
      case _: AssertionError =>
        var visited: DLocSet.T = DLocSet.bottom

        def adfs(l: DLoc.T): AAbsLoc = {
          if (l == DLoc.nullv) LocSetBot
          else if (visited.contains(l)) LocSetBot
          else {
            visited = visited + l

            val dn = s |> dlookup(l)
            val atag = dn |> ddlookup("className")

            val aa_n: AAbsLoc =
              if (atag |> isBottom) {
                // not an element
                LocSetBot
              } else {
                lazy val lset = dn |> ddlookup("obj") |> locset
                // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
                // TODO we ignore namespace

                lazy val b0 = hasNull(atag)

                // TODO Check the semantics. we have to split the following false case and join it to the result.
                lazy val b1 =
                  atag |> strval |> gamma_str match {
                    case Some(clses) =>
                      // TODO need to check
                      val clsess = clses.map(s => s.split(' '))
                      clses.exists(clsList => clsList.split(' ').exists(cls => clsName.equalsIgnoreCase(cls)))
                    case _ =>
                      if (b0) false
                      else throw new ModelError("TODO")
                  }

                if (atag |> pvalue |> isPBottom) {
                  LocSetBot
                } else if (b1) {
                  lset
                } else {
                  LocSetBot
                }
              }
            val firsts = firstChilds(l)(s)
            val nexts = nextSiblings(l)(s)
            val lset_1 = (LocSetBot /: firsts)((lset_i, first) => joinLocSet(lset_i, adfs(first)))
            val lset_2 = (LocSetBot /: nexts)((lset_i, next) => joinLocSet(lset_i, adfs(next)))
            joinLocSet(lset_1, joinLocSet(lset_2, aa_n))
          }
        }

        val lset = (LocSetBot /: fs)((lset_i, f) => joinLocSet(lset_i, adfs(f)))
        createHTMLCollection(lset)
    }
  }

  def getElementsByClassNameA(l_node: ALoc, aclsName: AAbsStr, l_r: ALoc)(s: AAbsState): AAbsState = {
    def dfs(l: DLoc.T, ith: Int, aa: Vector[AAbsLoc]): (Int, Vector[AAbsLoc]) = {
      if (l == DLoc.nullv) (ith, aa)
      else {
        val dn = s |> dlookup(l)
        val acls = dn |> ddlookup("className")

        val aa_n =
          if (acls |> isBottom) {
            // not an element
            aa
          } else {
            lazy val lset = dn |> ddlookup("obj") |> locset
            // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
            // TODO we ignore namespace

            lazy val cls = acls //._1
            lazy val b0 = hasNull(acls)

            // TODO Check the semantics. we have to split the following false case and join it to the result.
            lazy val b1 =
              cls |> strval |> gamma_str match {
                case Some(clses) =>
                  // TODO need to check
                  clses.exists(clsList => clsList.split(' ').exists(cls => aclsName |> isRelated(cls)))
                case _ =>
                  if (b0) false
                  else throw new ModelError("TODO")
              }

            if (cls |> pvalue |> isPBottom) {
              aa
            } else if (b1) {
              aa :+ lset
            } else {
              aa
            }
          }
        val firsts = firstChilds(l)(s)
        val nexts = nextSiblings(l)(s)
        assert(firsts.isSingleton)
        assert(nexts.isSingleton)
        val first = firsts.head
        val (nith, aa_2) = dfs(first, ith + 1, aa_n)

        val next = nexts.head
        val (nith2, aa_3) = dfs(next, nith + 1, aa_2)

        (nith2, aa_3)
      }
    }

    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))

    try {
      assert(fs.isSingleton)

      val first = fs.head
      val array = dfs(first, 0, Vector.empty[AAbsLoc])._2

      s |> supdate(l_r, createHTMLCollection(array))
    } catch {
      case _: AssertionError =>
        var visited: DLocSet.T = DLocSet.bottom

        def adfs(l: DLoc.T): AAbsLoc = {
          if (l == DLoc.nullv) LocSetBot
          else if (visited.contains(l)) LocSetBot
          else {
            visited = visited + l

            val dn = s |> dlookup(l)
            val atag = dn |> ddlookup("className")

            val aa_n =
              if (atag |> isBottom) {
                // not an element
                LocSetBot
              } else {
                lazy val lset = dn |> ddlookup("obj") |> locset
                // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
                // TODO we ignore namespace

                lazy val tag = atag //._1
                lazy val b0 = hasNull(atag)

                // TODO Check the semantics. we have to split the following false case and join it to the result.
                lazy val b1 =
                  atag |> strval |> gamma_str match {
                    case Some(clses) =>
                      // TODO need to check
                      val clsess = clses.map(s => s.split(' '))
                      clses.exists(clsList => clsList.split(' ').exists(cls => aclsName |> isRelated(cls)))
                    case _ =>
                      if (b0) false
                      else throw new ModelError("TODO")
                  }

                if (atag |> pvalue |> isPBottom) {
                  LocSetBot
                } else if (b1) {
                  lset
                } else {
                  LocSetBot
                }
              }
            val firsts = firstChilds(l)(s)
            val nexts = nextSiblings(l)(s)
            val lset_1 = (LocSetBot /: firsts)((lset_i, first) => joinLocSet(lset_i, adfs(first)))
            val lset_2 = (LocSetBot /: nexts)((lset_i, next) => joinLocSet(lset_i, adfs(next)))
            joinLocSet(lset_1, joinLocSet(lset_2, aa_n))
          }
        }

        val lset = (LocSetBot /: fs)((lset_i, f) => joinLocSet(lset_i, adfs(f)))
        s |> supdate(l_r, createHTMLCollection(lset))
    }
  }

  def getElementsByClassName(l_node: ALoc, clsName: String, l_r: ALoc)(s: AAbsState): AAbsState = {
    s |> supdate(l_r, getElementsByClassNameI(l_node, clsName)(s))
  }

  def getElementsByClassName: SemanticsFun = genAPI(si => {
    val l_r = si.newRecentAlloc(10)
    val ctxs = si.lset_this
    val atags = callToString(si)(si.getArg(0))
    val s =
      lsetFoldLeftL(ctxs)(StateBot)((state_i, ctx) => {
        (state_i /: atags)((state_i2, atag) => {
          atag |> gamma_str match {
            case Some(tags) if tags.size == 1 =>
              val tag = tags.head
              joinState(state_i2, getElementsByClassName(ctx, tag, l_r)(si.state))
            case _ =>
              joinState(state_i2, getElementsByClassNameA(ctx, atag, l_r)(si.state))
          }
        })
      })
    returnValueC(l_r |> LtoValue)(si.copyi(state = s))
  })

  def getElementsByTagNameI(l_node: ALoc, tagName: String)(s: AAbsState): AAbsObj = {
    def dfs(l: DLoc.T, ith: Int, aa: Vector[AAbsLoc]): (Int, Vector[AAbsLoc]) = {
      if (l == DLoc.nullv) (ith, aa)
      else {
        val dn = s |> dlookup(l)
        val atag = dn |> ddlookup("localName")

        val aa_n =
          if (atag |> isBottom) {
            // not an element
            aa
          } else {
            lazy val lset = dn |> ddlookup("obj") |> locset
            // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
            // TODO we ignore namespace

            lazy val b0 = hasNull(atag)

            // TODO Check the semantics. we have to split the following false case and join it to the result.
            lazy val b1 =
              atag |> strval |> gamma_str match {
                case Some(tags) =>
                  // TODO need to check
                  tags.exists(t => tagName.equalsIgnoreCase(t))
                case _ =>
                  if (b0) false
                  else throw new ModelError("TODO")
              }

            if (tagName.equalsIgnoreCase("*")) {
              aa :+ lset
            } else if (atag |> pvalue |> isPBottom) {
              aa
            } else if (b1) {
              aa :+ lset
            } else {
              aa
            }
          }
        val firsts = firstChilds(l)(s)
        val nexts = nextSiblings(l)(s)
        assert(firsts.isSingleton)
        assert(nexts.isSingleton)
        val first = firsts.head
        val (nith, aa_2) = dfs(first, ith + 1, aa_n)

        val next = nexts.head
        val (nith2, aa_3) = dfs(next, nith + 1, aa_2)

        (nith2, aa_3)
      }
    }

    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))

    try {
      assert(fs.isSingleton)

      val first = fs.head
      val array = {
        System.out.println(s"* Warning: Invalid tag name: $tagName")
        Vector.empty[AAbsLoc]
      }

      createHTMLCollection(array)
    } catch {
      case _: AssertionError =>
        var visited: DLocSet.T = DLocSet.bottom

        def adfs(l: DLoc.T): AAbsLoc = {
          if (l == DLoc.nullv) LocSetBot
          else if (visited.contains(l)) LocSetBot
          else {
            visited = visited + l

            val dn = s |> dlookup(l)
            val atag = dn |> ddlookup("localName")

            val aa_n =
              if (atag |> isBottom) {
                // not an element
                LocSetBot
              } else {
                lazy val lset = dn |> ddlookup("obj") |> locset
                // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
                // TODO we ignore namespace

                lazy val b0 = atag |> hasNull

                // TODO Check the semantics. we have to split the following false case and join it to the result.
                lazy val b1 =
                  atag |> strval |> gamma_str match {
                    case Some(tags) =>
                      // TODO need to check
                      tags.exists(t => tagName.equalsIgnoreCase(t))
                    case _ =>
                      if (b0) false
                      else throw new ModelError("TODO")
                  }

                if (tagName.equalsIgnoreCase("*")) {
                  lset
                } else if (atag |> pvalue |> isPBottom) {
                  LocSetBot
                } else if (b1) {
                  lset
                } else {
                  LocSetBot
                }
              }
            val firsts = firstChilds(l)(s)
            val nexts = nextSiblings(l)(s)
            val lset_1 = (LocSetBot /: firsts)((lset_i, first) => joinLocSet(lset_i, adfs(first)))
            val lset_2 = (LocSetBot /: nexts)((lset_i, next) => joinLocSet(lset_i, adfs(next)))
            joinLocSet(lset_1, joinLocSet(lset_2, aa_n))
          }
        }

        val lset = (LocSetBot /: fs)((lset_i, f) => joinLocSet(lset_i, adfs(f)))
        createHTMLCollection(lset)
    }
  }

  def getElementsByTagName(l_node: ALoc, tagName: String, l_r: ALoc)(s: AAbsState): AAbsState = {
    s |> supdate(l_r, getElementsByTagNameI(l_node, tagName)(s))
  }

  def getElementsByTagNameA(l_node: ALoc, atagName: AAbsStr, l_r: ALoc)(s: AAbsState): AAbsState = {
    // consider all the possible elements.
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    val dlset = collectDecendants(fs, _ => true)(s)
    val lset = (LocSetBot /: dlset)((lset_i, dl) => joinLocSet(lset_i, s |> dlookup(dl) |> ddlookup("obj") |> locset))
    val o = createHTMLCollection(lset)
    s |> supdate(l_r, o)
  }

  def getElementsByTagName: SemanticsFun = genAPI(si => {
    val l_r = si.newRecentAlloc(10)
    val ctxs = si.lset_this
    val atags = callToString(si)(si.getArg(0))
    val s =
      lsetFoldLeftL(ctxs)(StateBot)((state_i, ctx) => {
        (state_i /: atags)((state_i2, atag) => {
          atag |> gamma_str match {
            case Some(tags) if tags.size == 1 =>
              val tag = tags.head
              joinState(state_i2, getElementsByTagName(ctx, tag, l_r)(si.state))
            case _ =>
              joinState(state_i2, getElementsByTagNameA(ctx, atag, l_r)(si.state))
          }
        })
      })
    returnValueC(l_r |> LtoValue)(si.copyi(state = s))
  })

  def getElementsByName(l_node: ALoc, clsName: String, l_r: ALoc)(s: AAbsState): AAbsState = {
    def dfs(l: DLoc.T, ith: Int, aa: Vector[AAbsLoc]): (Int, Vector[AAbsLoc]) = {
      if (l == DLoc.nullv) (ith, aa)
      else {
        val dn = s |> dlookup(l)
        val acls = dn |> ddlookup("name")

        val aa_n =
          if (acls |> isBottom) {
            // not an element
            aa
          } else {
            lazy val lset = dn |> ddlookup("obj") |> locset
            // @see: https://dom.spec.whatwg.org/#concept-getelementsbytagname
            // TODO we ignore namespace

            //            lazy val cls = acls._1
            lazy val b0 = acls |> hasNull

            // TODO Check the semantics. we have to split the following false case and join it to the result.
            lazy val b1 =
              acls |> strval |> gamma_str match {
                case Some(clses) =>
                  // TODO need to check
                  clses.exists(clsList => clsList.split(' ').exists(cls => clsName.equalsIgnoreCase(cls)))
                case _ =>
                  if (b0) false
                  else throw new ModelError("TODO")
              }

            if (acls |> pvalue |> isPBottom) {
              aa
            } else if (b1) {
              aa :+ lset
            } else {
              aa
            }
          }
        val firsts = firstChilds(l)(s)
        val nexts = nextSiblings(l)(s)
        assert(firsts.isSingleton)
        assert(nexts.isSingleton)
        val first = firsts.head
        val (nith, aa_2) = dfs(first, ith + 1, aa_n)

        val next = nexts.head
        val (nith2, aa_3) = dfs(next, nith + 1, aa_2)

        (nith2, aa_3)
      }
    }

    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    assert(fs.isSingleton)

    val first = fs.head
    val (nith, array) = dfs(first, 0, Vector.empty[AAbsLoc])

    s |> supdate(l_r, createHTMLCollection(array))
  }

  def getElementsByNameA(l_node: ALoc, aclsName: AAbsStr, l_r: ALoc)(s: AAbsState): AAbsState = {
    def hasName(dl: DLoc.T): Boolean = {
      !(s |> dlookup(dl) |> ddlookup("name") |> isBottom)
    }

    val o = {
      // consider all the possible elements.
      val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
      val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
      val dlset = collectDecendants(fs, hasName)(s)
      val lset = (LocSetBot /: dlset)((lset_i, dl) => joinLocSet(lset_i, s |> dlookup(dl) |> ddlookup("obj") |> locset))
      createHTMLCollection(lset)
    }
    s |> supdate(l_r, o)
  }

  def getElementsByName: SemanticsFun = genAPI(si => {
    val l_r = si.newRecentAlloc(10)
    val ctxs = si.lset_this
    val atags = callToString(si)(si.getArg(0))
    val s =
      lsetFoldLeftL(ctxs)(StateBot)((state_i, ctx) => {
        (state_i /: atags)((state_i2, atag) => {
          atag |> gamma_str match {
            case Some(tags) if tags.size == 1 =>
              val tag = tags.head
              joinState(state_i2, getElementsByName(ctx, tag, l_r)(si.state))
            case _ =>
              joinState(state_i2, getElementsByNameA(ctx, atag, l_r)(si.state))
          }
        })
      })
    returnValueC(l_r |> LtoValue)(si.copyi(state = s))
  })

  def getElementById(l_node: ALoc, aid: AAbsStr)(s: AAbsState): AAbsValue = {
    var visited = DLocSet.bottom

    def dfs(l: DLoc.T): AAbsValue = {
      if (l == DLoc.nullv) AAbsValueNull
      else {
        if (visited.contains(l)) ValueBot
        else {
          visited = visited + l
          lazy val o = s |> dlookup(l)
          lazy val v = o |> ddlookup("obj")
          // @see: https://dom.spec.whatwg.org/#dom-nonelementparentnode-getelementbyid
          lazy val ids = o |> ddlookup("id") //._1
          lazy val c_1 = ids |> pvalue |> isPBottom
          lazy val c_2 = orderBool(BoolTrue, aid |> StrictEquals(ids |> strval))
          lazy val c_3 = aid |> isARelated(ids |> strval)
          lazy val s_cond = !c_1 && c_2
          lazy val o_cond = !c_1 && c_3

          if ((aid |> isSingleStr) && s_cond) v
          else {
            val v_1 =
              if (o_cond) v
              else ValueBot

            val firsts = firstChilds(l)(s)
            val v_2 = (ValueBot /: firsts)((v_i, first) => joinValue(v_i, dfs(first)))
            if (v_2 |> hasNull) {
              val nexts = nextSiblings(l)(s)
              val v_3 = (ValueBot /: nexts)((v_i, next) => joinValue(v_i, dfs(next)))
              joinValue(v_1, joinValue(v_2 |> toPBot, v_3))
            } else {
              joinValue(v_1, v_2)
            }
          }
        }
      }
    }

    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))

    (ValueBot /: fs)((v_i, first) => joinValue(v_i, dfs(first)))
  }

  def querySelectorAllA(l_node: ALoc, aselector: AAbsStr, l_r: ALoc)(s: AAbsState): (AAbsState, Set[AException]) = {
    // consider all the possible elements.
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    val dlset = collectDecendants(fs, _ => true)(s)
    val lset = (LocSetBot /: dlset)((lset_i, dl) => joinLocSet(lset_i, s |> dlookup(dl) |> ddlookup("obj") |> locset))
    val o = createHTMLCollection(lset)
    val es = AExceptionBot + ADOMError

    (s |> supdate(l_r, o), es)
  }

  def querySelectorAll(l_node: ALoc, selector: String, l_r: ALoc)(s: AAbsState): (AAbsState, Set[AException]) = {
    //    try {
    //      val sels = parseSelectors(selector)
    //      if (sels.size == 1) {
    //        val sel = sels.head
    //        val s_n =
    //          sel match {
    //            case i: ConditionalSelectorImpl =>
    //              i.getCondition match {
    //                case f: ClassConditionImpl =>
    //                  val cls = f.getValue
    //                  getElementsByClassName(l_node, cls, l_r)(s)
    //                case f: IdConditionImpl =>
    //                  val id = f.getValue
    //                  val v = getElementById(l_node, AbsString.constant(id))(s)
    //                  val o_0 =
    //                    if (Value(NullTop) <= v) createHTMLCollection(Vector.empty[LocSet])
    //                    else AbsObj.bottom
    //                  val o_n =
    //                    if (v._2.nonEmpty) createHTMLCollection(Vector.empty[LocSet] :+ v._2)
    //                    else AbsObj.bottom
    //                  s.update(l_r, o_0 + o_n)
    //                case _ =>
    //                  i.getSimpleSelector match {
    //                    case e: ElementSelectorImpl =>
    //                      val tag = e.getLocalName
    //                      // TODO val attr = f.getValue
    //                      getElementsByTagName(l_node, tag, l_r)(s)
    //                    case _ =>
    //                      System.err.println(s"* Warning: querySelectorAll: TODO '$selector'")
    //                      val inodes = s(l_node).lookupi("@node")._1._1.dlocset
    //                      val fs = (DLocSet.bottom /: inodes) ((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    //                      val dlset = collectDecendants(fs, _ => true)(s)
    //                      val lset = (LocSet.empty /: dlset) ((lset_i, dl) => lset_i ++ s.dlookup(dl).lookup("obj")._2)
    //                      System.out.println(lset.toString)
    //                      s.update(l_r, createHTMLCollection(lset))
    //                  }
    //              }
    //            case i: ElementSelectorImpl if i.getLocalName == null =>
    //              // *
    //              getElementsByTagName(l_node, "*", l_r)(s)
    //            case i: ElementSelectorImpl =>
    //              // tagname
    //              val tag = i.getLocalName
    //              getElementsByTagName(l_node, tag, l_r)(s)
    //
    //            case _ =>
    //              System.err.println(s"* Warning: querySelectorAll: TODO '$selector'")
    //              val inodes = s(l_node).lookupi("@node")._1._1.dlocset
    //              val fs = (DLocSet.bottom /: inodes) ((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    //              val dlset = collectDecendants(fs, _ => true)(s)
    //              val lset = (LocSet.empty /: dlset) ((lset_i, dl) => lset_i ++ s.dlookup(dl).lookup("obj")._2)
    //              s.update(l_r, createHTMLCollection(lset))
    //          }
    //
    //        (s_n, ExceptionBot)
    //      } else {
    querySelectorAllA(l_node, StrTop, l_r)(s)
    //      }
    //    } catch {
    //      case e: CSSParseException =>
    //        (State.bottom, ExceptionBot + DOMError)
    //    }
  }

  def querySelectorAll: SemanticsFun = {
    genAPI(si => {
      val l_r = si.newAlloc(10)
      val ctxs = si.lset_this
      var es: HashSet[AException] = AExceptionBot
      val atags = callToString(si)(si.getArg(0))
      val s =
        lsetFoldLeftL(ctxs)(StateBot)((state_i, ctx) => {
          (state_i /: atags)((state_i2, atag) => {
            atag |> gamma_str match {
              case Some(tags) =>
                (state_i2 /: tags)((state_i3, tag) => {
                  val (s_n, es_n) = querySelectorAll(ctx, tag, l_r)(si.state)
                  es ++= es_n
                  joinState(state_i3, s_n)
                })
              case _ =>
                val (s_n, es_n) = querySelectorAllA(ctx, atag, l_r)(si.state)
                es ++= es_n
                joinState(state_i2, s_n)
            }
          })
        })
      val ess = exception(es)(si.state)
      returnValueD(l_r |> LtoValue, ess)(si.copyi(state = s))
    })
  }

  def querySelector(l_node: ALoc, selector: String)(s: AAbsState): (AAbsValue, Set[AException]) = {
    //    try {
    //      val sels = parseSelectors(selector)
    //      assert(sels.size == 1)
    //      val sel = sels.head
    //      val v_n =
    //        sel match {
    //          case i: ConditionalSelectorImpl =>
    //            i.getCondition match {
    //              case f: ClassConditionImpl =>
    //                val cls = f.getValue
    //                getElementsByClassNameI(l_node, cls)(s).lookup(AbsString.constant(0))._1._1
    //              case f: IdConditionImpl =>
    //                val id = f.getValue
    //                getElementById(l_node, AbsString.constant(id))(s)
    //              case _ =>
    //                System.err.println(s"* Warning: querySelector: TODO '$selector'")
    //                val inodes = s(l_node).lookupi("@node")._1._1.dlocset
    //                val fs = (DLocSet.bottom /: inodes) ((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    //                val dlset = collectDecendants(fs, _ => true)(s)
    //                val lset = (LocSet.empty /: dlset) ((lset_i, dl) => lset_i ++ s.dlookup(dl).lookup("obj")._2)
    //                Value(lset)
    //            }
    //          case i: ElementSelectorImpl if i.getLocalName == null =>
    //            // *
    //            // TODO firstChilds
    //            getNamedNode(DocumentElementNode)
    //          case i: ElementSelectorImpl =>
    //            // tagname
    //            val tag = i.getLocalName
    //            getElementsByTagNameI(l_node, tag)(s).lookup(AbsString.constant("0"))._1._1
    //
    //          case _ =>
    System.err.println(s"* Warning: querySelectorAll: TODO '$selector'")
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    val dlset = collectDecendants(fs, _ => true)(s)
    val v_n = (ValueBot /: dlset)((lset_i, dl) => joinValue(lset_i, s |> dlookup(dl) |> ddlookup("obj")))

    (v_n, AExceptionBot + ADOMError)
    //        }
    //      (v_n, ExceptionBot)
    //    } catch {
    //      case _: CSSParseException =>
    //        (Value.bottom, ExceptionBot + DOMError)
    //    }
  }

  def querySelectorA(l_node: ALoc, aselector: AAbsStr)(s: AAbsState): (AAbsValue, Set[AException]) = {
    // consider all the possible elements.
    val inodes = s |> slookup(l_node) |> lookupi(ii("Node")) |> ItoValue |> dlocset
    val fs = (DLocSet.bottom /: inodes)((dlocs_i, inode) => dlocs_i + firstChilds(inode)(s))
    val dlset = collectDecendants(fs, _ => true)(s)
    val lset = (ValueBot /: dlset)((lset_i, dl) => joinValue(lset_i, s |> dlookup(dl) |> ddlookup("obj")))
    (lset, AExceptionBot + ADOMError)
  }

  def querySelector: SemanticsFun = {
    genAPI(si => {
      val ctxs = si.lset_this
      var es: HashSet[AException] = AExceptionBot
      val atags = callToString(si)(si.getArg(0))
      val v =
        lsetFoldLeftL(ctxs)(ValueBot)((v_i, ctx) => {
          (v_i /: atags)((v_i2, atag) => {
            atag |> gamma_str match {
              case Some(tags) =>
                (v_i2 /: tags)((v_i3, tag) => {
                  val (v_n, es_n) = querySelector(ctx, tag)(si.state)
                  es ++= es_n
                  joinValue(v_i3, v_n)
                })
              case _ =>
                val (v_n, es_n) = querySelectorA(ctx, atag)(si.state)
                es ++= es_n
                joinValue(v_i2, v_n)
            }
          })
        })

      val ess = exception(es)(si.state)
      returnValueD(v, ess)(si)
    })
  }

  def getElementById: SemanticsFun = genAPI(si => {
    val ctxs = si.lset_this
    val aids = callToString(si)(si.getArg(0))
    val v =
      lsetFoldLeftL(ctxs)(ValueBot)((v_i, ctx) => {
        (v_i /: aids)((v_i2, aid) => joinValue(v_i2, getElementById(ctx, aid)(si.state)))
      })

    returnValueC(v)(si)
  })

  def matchesSelector(l_node: ALoc, selector: String)(s: AAbsState): (AAbsBool, Set[AException]) = {
    //    try {
    if (selector.equalsIgnoreCase(":hidden")) (BoolTop, AExceptionBot + ADOMError)
    else {
      //        val sels = parseSelectors(selector)
      //        assert(sels.size == 1)
      //        val sel = sels.head
      val b =
        //          sel match {
        //            case i: ElementSelectorImpl =>
        //              // tagname
        //              val tagName = i.getLocalName
        //              val inodes = s(l_node).lookupi("@node")._1._1.dlocset
        //              (AbsBool.bottom /: inodes) ((b_i, l) => {
        //                val dn = s.dlookup(l)
        //                val atag = dn.lookup("localName")
        //                lazy val tag = atag._1
        //                lazy val b0 = NullTop <= tag._2
        //                lazy val b1 =
        //                  tag._5.gamma match {
        //                    case Some(tags) =>
        //                      // TODO need to check
        //                      tags.exists(t => tagName.equalsIgnoreCase(t))
        //                    case _ =>
        //                      if (b0) false
        //                      else throw new ModelError("TODO")
        //                  }
        //
        //                val b_n =
        //                  if (tagName.equalsIgnoreCase("*")) {
        //                    BoolTrue
        //                  } else {
        //                    AbsBool.alpha(b1)
        //                  }
        //                b_i + b_n
        //              })
        //            case _ =>
        //              System.err.println(s"* Warning: TODO: matchSelector: $selector")
        BoolTop
      //          }
      (b, AExceptionBot)
    }
    //    } catch {
    //      case e: CSSParseException =>
    //        System.out.println(s"* Info: matchesSelector CSSParseException $e")
    //        (AbsBool.bottom, ExceptionBot + DOMError)
    //    }
  }

  def XMLHttpRequestSend: SemanticsFun = genAPI(si => throw new InternalError("TODO"))

  def getPropertyValue(lset_elems: AAbsLoc, name: String, i: Long)(s: AAbsState): AAbsValue = {
    val lset_style = lsetFoldLeftL(lset_elems)(ValueBot)((lset_i, l) => joinValue(lset_i, s |> slookup(l) |> lookupi(ii("Style")) |> ItoValue))

    val rtn = name match {
      case "display" =>
        lsetFoldLeft(lset_style)(ValueBot)((s_i, l) => {
          val o = s |> slookup(l)
          val v = o |> lookup(toAbsStr(name))
          val v_2 =
            if ((toAbsStr("") |> StrictEquals(v |> strval)) |> has(true)) joinValue(v, toAbsStr("block") |> StoValue)
            else v
          joinValue(s_i, v_2)
        })
      case _ =>
        OtherStr |> StoValue
    }
    rtn
  }

  def makeString(v: AAbsValue)(si: SFInput): AAbsValue = {
    val sset: Set[AAbsStr] = callToString(si)(v)

    // Note: Type Check.
    // We consider simplified semantics that checks whether the object has a corresponding DOM object or not.

    (ValueBot /: sset)((s_i, s_n) => joinValue(s_i, s_n |> StoValue))
  }

  def makeBool(v: AAbsValue)(si: SFInput): AAbsValue = v |> toBoolean |> BtoValue

  def assignDProp(prop: String, process: AAbsValue => SFInput => AAbsValue)(si: SFInput): (AAbsState, AAbsState, AAbsValue) = {
    val v = si.getArg(0)
    val v_revised = process(v)(si)
    val (s_2, es) = assignDPropValue(prop, v_revised)(si)

    val nes = if (es.nonEmpty) exception(es)(si.state) else StateBot

    (s_2, nes, v_revised)
  }

  def assignDPropValue(prop: String, v_revised: AAbsValue)(si: SFInput): (AAbsState, Set[AException]) = {
    val s = si.state

    val lset_this = si.lset_this
    val (dlset_ok, b_notok) = getDNodes(s, lset_this)

    val s_2 =
      (StateBot /: dlset_ok)((s_i, dl) => joinState(s_i, s |> dupdate(dl, s |> dlookup(dl) |> dupdate(prop, v_revised))))

    val lset_attr =
      lsetFoldLeftL(lset_this)(ValueBot)((lset_i, l) => {
        joinValue(lset_i, s |> slookup(l) |> lookupi(ii("Attributes")) |> ItoValue)
      })

    val s_3 =
      lsetFoldLeft(lset_attr)(s_2)((s_i, l) => {
        val o = s_i |> slookup(l)
        s_i |> supdate(l, o |> update(toAbsStr(prop), toDataProp(v_revised)))
      })

    val es =
      if (b_notok) AExceptionBot + AError
      else AExceptionBot

    (s_3, es)
  }

  def assignStringDProp(prop: String)(si: SFInput): (AAbsState, AAbsState, AAbsValue) = {
    assignDProp(prop, makeString)(si)
  }

  def assignIProp(prop: AIName)(si: SFInput): (AAbsState, AAbsState, AAbsValue) = {
    val s = si.state
    val v = si.getArg(0)

    val (lset_ok, bnotok) = lsetFoldLeftL(si.lset_this)((HashSet.empty[ALoc], false))((lb_i, l) => {
      val dom = s |> slookup(l) |> domIni(prop)
      val lset = if (dom |> has(true)) lb_i._1 + l else lb_i._1
      val b = lb_i._2 || (dom |> has(false))
      (lset, b)
    })

    val s_n = (s /: lset_ok)((s_i, l) => joinState(s_i, s |> supdate(l, s |> slookup(l) |> updatei(prop, v |> toIValue))))

    val es =
      if (bnotok) exception(AExceptionBot + AError)(si.state)
      else StateBot

    (s_n, es, AAbsValueNull)
  }

  def removeDPropValue(prop: String)(si: SFInput): (AAbsState, Set[AException]) = {
    val s = si.state

    val lset_this = si.lset_this
    val (dlset_ok, b_notok) = getDNodes(s, lset_this)

    val s_2 = (StateBot /: dlset_ok)((s_i, dl) => joinState(s_i, s |> dupdate(dl, (s |> dlookup(dl)).remove(prop))))

    val es =
      if (b_notok) AExceptionBot + AError
      else AExceptionBot

    (s_2, es)
  }

  def getIProp(prop: AIName)(s: SFInput): (AAbsValue, Set[AException]) = {
    val (lset_ok, lset_notok) = lsetFoldLeftL(s.lset_this)((HashSet.empty[ALoc], HashSet.empty[ALoc])) {
      (ll_i, l) =>
        val d = s.state |> slookup(l) |> domIni(prop)
        val lset1 = if (orderBool(BoolTrue, d)) ll_i._1 + l else ll_i._1
        val lset2 = if (orderBool(BoolFalse, d)) ll_i._2 + l else ll_i._2
        (lset1, lset2)
    }

    val pv = (ValueBot /: lset_ok)((v_i, l) => joinValue(v_i, s.state |> slookup(l) |> lookupi(prop) |> ItoValue))
    val es = if (lset_notok.nonEmpty) AExceptionBot + AError else AExceptionBot

    (pv, es)
  }

  def DOM2EventAdd(name: String): SemanticsFun = genAPI(si => {
    val handler = si.getArg(0)
    val ctxs = si.lset_this
    val s = si.state
    val handlers = filterCallable(s, handler)

    val (iset_ok, b_nok) = getDNodes(s, ctxs)

    val s_n =
      (StateBot /: iset_ok)((s_i, inode) => {
        val o = s |> dlookup(inode)
        val dn =
          lsetFoldLeft(handlers)(o)((o_i, l) => {
            o_i |> addEventListener(name, l, bubble = false)
          })
        s |> dupdate(inode, dn |> dupdate(name, handlers))
      })

    val es =
      if (b_nok) exception(Set.empty[AException] + AError)(si.state)
      else StateBot

    returnValueD(handler, es)(si.copyi(state = s_n))
  })

  def genObject(l_r: ALoc, cls: String, l_proto: ALoc, f: AAbsObj => AAbsObj)(s: SFInput): SFInput = {
    val s_1 = oldify(l_r)(s.state)
    val o_new = newObject(l_proto, cls)
    val s_2 = s_1 |> supdate(l_r, f(o_new))
    s.copyi(state = s_2)
  }

  def returnDOMProp(prop: String): SemanticsFun = {
    genAPI(si => {
      val (v, es) = getDPropOfThis(prop)(si)
      val ess = exception(es)(si.state)
      returnValueD(v, ess)(si)
    })
  }

  def returnDOMPropElem(prop: String): SemanticsFun = {
    genAPI(si => {
      val (v, es) = getDPropOfThis(prop)(si)
      val dl = v |> dlocset
      val vl_1 = if (dl.hasNull) AAbsValueNull else ValueBot
      val vl = (ValueBot /: dl.nonNulls)((lset_i, dl) => joinValue(lset_i, si.state |> dlookup(dl) |> ddlookup("obj")))
      val ess = exception(es)(si.state)
      returnValueD(joinValue(vl, vl_1), ess)(si)
    })
  }

  def returnInternalProp(prop: AIName): SemanticsFun = {
    genAPI(si => {
      val (v, es) = getIProp(prop)(si)
      val ess = exception(es)(si.state)
      returnValueD(v, ess)(si)
    })
  }

  def genSemantics(): String => (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue) = {
    val ssSemantics: Map[String, SemanticsFun] = HashMap(
      "console.log" -> genAPI(si => {
        System.err.println(s"- console.log: '${si.getArg(0)}'")
        returnUndef(si)
      }),
      "chrome.app.getIsInstalled" -> returnValueAPI(false),

      "Navigator.prototype.appCodeName$get" -> returnValueAPI(appCodeName),
      "Navigator.prototype.appName$get" -> returnValueAPI(appName),
      "Navigator.prototype.appVersion$get" -> returnValueAPI(appVersion),
      "Navigator.prototype.userAgent$get" -> returnValueAPI(userAgent),
      "Navigator.prototype.platform$get" -> returnValueAPI(platform),
      "Navigator.prototype.product$get" -> returnValueAPI(product),
      "Navigator.prototype.cookieEnabled$get" -> returnValueAPI(true),
      "Navigator.prototype.language$get" -> returnValueAPI("ko-KR"),

      "Screen.prototype.colorDepth$get" -> returnValueAPI(NtoValue(UInt)),
      "Screen.prototype.height$get" -> returnValueAPI(NtoValue(UInt)),
      "Screen.prototype.width$get" -> returnValueAPI(NtoValue(UInt)),

      "History.prototype.length$get" -> returnValueAPI(NtoValue(UInt)),

      "document.location$get" -> returnValueLocName("#location"),
      "document.location$set" ->
        genAPI(si => {
          System.err.println("Ignored attempts to update the value of 'document.location' property")
          returnUndef(si)
        }),

      "Node.prototype.compareDocumentPosition" -> returnValueIRange(0, 64),

      "setInterval" -> genAPI(si => {
        val s: AAbsState = si.state
        val handler: AAbsValue = si.getArg(0)
        val handlers = filterCallable(s, handler)

        assert(isNonEmpty(handlers))

        val addr = si.newAlloc(1)
        val (v, s_n) = setInterval(handlers, addr)(s)
        returnValueC(v)(si.copyi(state = s_n))
      }),
      "clearInterval" -> genAPI(si => {
        val s: AAbsState = si.state
        val handle: AAbsValue = si.getArg(0)

        val s_n =
          if (handle |> isSingleLoc)
            clearInterval(handle |> getSingleLoc)(s)
          else s

        returnUndef(si.copyi(state = s_n))
      }),
      "setTimeout" -> genAPI(si => {
        val s: AAbsState = si.state
        val handler: AAbsValue = si.getArg(0)
        val handlers = filterCallable(s, handler)

        assert(isNonEmpty(handlers))

        val addr = si.newAlloc(1)
        val (v, s_n) = setInterval(handlers, addr)(s)
        returnValueC(v)(si.copyi(state = s_n))
      }),

      "getComputedStyle" -> genAPI(si => {
        val addr = si.newRecentAlloc(10)
        val elems = si.getArg(0)
        val pseudo = si.getArg(1)
        val (ps_0, ps_1) = filter(pseudo)
        val ps = toDOMString(ps_0)
        val ps_2 =
          if (ps_1 |> isBottom) ps
          else ps + null
        assert(ps_2.size == 1)
        val s_n = getComputedStyle(elems, ps_2.head, addr)(si.state)

        returnValueC(LtoValue(addr))(si.copyi(state = s_n))
      }),

      "EventTarget.prototype.addEventListener" -> genAPI(si => {
        val ctxs = si.lset_this
        val s = si.state
        val name = si.getArg(0)
        val handler = si.getArg(1)
        val names: Set[AAbsStr] = callToString(si)(name)
        val handlers = filterCallable(s, handler)

        val (iset_ok, b_nok) = getDNodes(s, ctxs)
        assert(!b_nok)
        val s_n =
          if (!(handlers |> isBottom)) {
            (StateBot /: iset_ok)((s_i, inode) => {
              (s_i /: names)((s_i2, name) => {
                name |> gamma_str match {
                  case Some(cnames) =>
                    (s_i2 /: cnames)((s_i3, cname) => {
                      lsetFoldLeft(handlers)(s_i3)((s_i4, h) => {
                        val dn: DNode.T = s |> dlookup(inode) |> addEventListener(cname, h, bubble = false)
                        joinState(s_i4, s |> dupdate(inode, dn))
                      })
                    })
                  case _ =>
                    System.err.println(si)
                    throw new InternalError(s"TODO: $name and $handlers")
                }
              })
            })
          } else s
        returnUndef(si.copyi(state = s_n))
      }),
      "EventTarget.prototype.removeEventListener" -> genAPI(si => {
        val ctxs = si.lset_this
        val s = si.state
        val name = si.getArg(0)
        val handler = si.getArg(1)
        val names: Set[AAbsStr] = callToString(si)(name)
        val handlers = filterCallable(s, handler)

        val (iset_ok, b_nok) = getDNodes(s, ctxs)
        assert(!b_nok)
        val s_n =
          if (!(handlers |> isBottom)) {
            (StateBot /: iset_ok)((s_i, inode) => {
              (s_i /: names)((s_i2, name) => {
                name |> gamma_str match {
                  case Some(cnames) =>
                    (s_i2 /: cnames)((s_i3, cname) => {
                      lsetFoldLeft(handlers)(s_i3)((s_i4, h) => {
                        val dn: DNode.T = s |> dlookup(inode) |> removeEventListener(cname, h, bubble = false)
                        joinState(s_i4, s |> dupdate(inode, dn))
                      })
                    })
                  case _ =>
                    System.err.println(si)
                    throw new InternalError(s"TODO: $name and $handlers")
                }
              })
            })
          } else s
        returnUndef(si.copyi(state = s_n))
      }),

      "Image" -> genAPI(si => {
        val v_doc = LStoValue(si.lset_this)
        if (si.lenArg |> isNonZero) {
          System.err.println("* Warning: arguments of Image constructor are ignored.")
          // TODO should be "width" and "height" as number type value.
        }
        val (v, s_n) = createElement("img", v_doc)(si)

        returnValueC(v)(si.copyi(state = s_n))
      }),

      "Document.prototype.createElement" -> genAPI(si => {
        val name = callToString(si)(si.getArg(0))
        val v_doc = si.v_this

        val (v, s_n) =
          ((ValueBot, StateBot) /: name)((s_i, n) => {
            val (v_i, s_n) =
              gamma_str(n) match {
                case Some(strs) if strs.size == 1 =>
                  createElement(strs.head, v_doc)(si)
                case _ =>
                  System.err.println(s"* Warning: ignore abstract tag name case: $n")
                  createElement("DIV", v_doc)(si)
              }
            (joinValue(s_i._1, v_i), joinState(s_i._2, s_n))
          })

        returnValueC(v)(si.copyi(state = s_n))
      }),
      "Document.prototype.createDocumentFragment" -> genAPI(si => {
        val (l_r, s_n) = createDocumentFragment()(si.state, si.newAlloc, si.newDAlloc)
        returnValueC(l_r |> LtoValue)(si.copyi(state = s_n))
      }),

      "Document.prototype.readyState$get" -> returnDOMProp("readyState"),
      "Document.prototype.compatMode$get" -> returnValueAPI(compatMode),
      "Document.prototype.documentElement$get" ->
        genAPI(si => returnValueC(getNamedNode(DocumentElementNode))(si)),
      "Document.prototype.body$get" -> genAPI(si => returnValueC(getNamedNode(BodyNode))(si)),
      "Document.prototype.head$get" -> genAPI(si => returnValueC(getNamedNode(HeadNode))(si)),
      "Document.prototype.forms$get" -> genAPI(si => {
        val l_r = si.newRecentAlloc(10)
        val ctxs = si.lset_this
        val s = lsetFoldLeftL(ctxs)(StateBot)((state_i, ctx) => joinState(state_i, forms_get(ctx, l_r)(si.state)))

        returnValueC(LtoValue(l_r))(si.copyi(state = s))
      }),
      "Document.prototype.defaultView$get" -> genAPI(si => { returnValueC(GlobalLoc |> LtoValue)(si) }),
      "Document.prototype.getElementsByClassName" -> getElementsByClassName,
      "Document.prototype.getElementsByTagName" -> getElementsByTagName,
      "Document.prototype.getElementsByName" -> getElementsByName,
      "Document.prototype.getElementById" -> getElementById,
      "Document.prototype.createTextNode" -> genAPI(si => {
        val arg = si.getArg(0)
        val name = callToString(si)(arg)

        val (lset, s_n) =
          ((ValueBot, StateBot) /: name)((s_i, n) => {
            val (l_r, s_n) = createTextNode(n)(si.state, si.newRecentAlloc, si.newDAlloc)
            (joinValue(s_i._1, l_r |> LtoValue), joinState(s_i._2, s_n))
          })

        returnValueC(lset)(si.copyi(state = s_n))
      }),
      "Document.prototype.createComment" -> genAPI(si => {
        val name = callToString(si)(si.getArg(0))
        val (lset, s_n) =
          ((ValueBot, StateBot) /: name)((s_i, n) => {
            val (l_r, s_n) = createComment(n)(si)
            (joinValue(s_i._1, l_r |> LtoValue), joinState(s_i._2, s_n))
          })

        returnValueC(lset)(si.copyi(state = s_n))
      }),
      // TODO
      "Document.prototype.querySelectorAll" -> querySelectorAll,
      "Document.prototype.querySelector" -> querySelector,
      "Document.prototype.close" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.write" -> genAPI(si => {
        val v = callToString(si)(si.getArg(0))
        System.err.println(s"* Warning: Ignored the semantics of document.write($v)")
        returnUndef(si)
      }),
      "Document.prototype.onclick$set" -> DOM2EventAdd("click"),
      "Document.prototype.onclick$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onselect$set" -> DOM2EventAdd("select"),
      "Document.prototype.onselect$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onselectstart$set" -> DOM2EventAdd("selectstart"),
      "Document.prototype.onselectstart$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onkeydown$set" -> DOM2EventAdd("keydown"),
      "Document.prototype.onkeydown$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onkeypress$set" -> DOM2EventAdd("keypress"),
      "Document.prototype.onkeypress$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onpaste$set" -> DOM2EventAdd("paste"),
      "Document.prototype.onpaste$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onmousedown$set" -> DOM2EventAdd("mousedown"),
      "Document.prototype.onmousedown$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onmousemove$set" -> DOM2EventAdd("mousemove"),
      "Document.prototype.onmousemove$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onmouseup$set" -> DOM2EventAdd("mouseup"),
      "Document.prototype.onmouseup$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.onmousewheel$set" -> DOM2EventAdd("mousewheel"),
      "Document.prototype.onmousewheel$get" -> returnValueAPI(AAbsValueNull),
      "Document.prototype.cookie$set" -> genAPI(si => {
        System.err.println("* Warning: TODO: update cookie value")
        assignStringDProp("cookie")(si)
      }),
      "Document.prototype.cookie$get" -> returnDOMProp("cookie"),
      "Document.prototype.referrer$get" -> returnValueAPI(OtherStr |> StoValue),

      "DocumentFragment.prototype.getElementById" -> getElementById,

      "Element.prototype.getElementsByTagName" -> getElementsByTagName,
      "Element.prototype.getElementsByClassName" -> getElementsByClassName,
      "Element.prototype.getElementsByName" -> getElementsByName,
      "Element.prototype.webkitMatchesSelector" -> genAPI(si => {
        val ctxs = si.lset_this
        val atags = callToString(si)(si.getArg(0))
        var es: HashSet[AException] = AExceptionBot
        val s =
          lsetFoldLeftL(ctxs)(BoolBot)((state_i, ctx) => {
            (state_i /: atags)((state_i2, atag) => {
              atag |> gamma_str match {
                case Some(tags) if tags.size == 1 =>
                  val tag = tags.head
                  val (s_n, es_n) = matchesSelector(ctx, tag)(si.state)
                  es ++= es_n
                  joinBool(state_i2, s_n)
                case _ =>
                  System.err.println(s"* Info: webkitMatchesSelector: unknown argument $atags")
                  es += ADOMError
                  BoolTop
              }
            })
          })
        val s_n =
          if (s |> isBBottom) StateBot
          else si.state

        val ess = exception(es)(si.state)
        returnValueD(s |> BtoValue, ess)(si.copyi(state = s_n))
      }),
      "Element.prototype.id$set" -> genAPI(si => assignStringDProp("id")(si)),
      "Element.prototype.id$get" -> returnDOMProp("id"),
      "Element.prototype.tagName$get" -> returnDOMProp("localName"),
      "Element.prototype.clientWidth$get" -> returnDOMProp("clientWidth"),
      "Element.prototype.clientHeight$get" -> returnDOMProp("clientHeight"),
      "Element.prototype.clientLeft$get" -> returnDOMProp("clientLeft"),
      "Element.prototype.clientTop$get" -> returnDOMProp("clientTop"),
      "Element.prototype.scrollWidth$get" -> returnDOMProp("scrollWidth"),
      "Element.prototype.scrollHeight$get" -> returnDOMProp("scrollHeight"),
      "Element.prototype.scrollLeft$get" -> returnDOMProp("scrollLeft"),
      "Element.prototype.scrollTop$get" -> returnDOMProp("scrollTop"),
      "Element.prototype.className$set" -> genAPI(si => { assignStringDProp("className")(si) }),
      "Element.prototype.className$get" -> returnDOMProp("className"),
      "Element.prototype.innerHTML$get" -> returnValueAPI(StrTop |> StoValue),
      "Element.prototype.innerHTML$set" -> genAPI(si => {
        val new_values = callToString(si)(si.getArg(0))
        val context_object = si.lset_this

        val result_states =
          (StateBot /: new_values) {
            case (states_i, new_value) =>

              // TODO Let _fragment_ be the result of invoking *fragmentParsing* with _new value_ and _context object_.
              val states = StateBot // fragmentParsing(new_value, context_object)(si)

              joinState(states_i, states)
          }

        (result_states, StateBot, ValueBot) // TODO
      }),
      "Element.prototype.outerHTML$get" -> genAPI(si => {
        val (v, es) = getDPropOfThis("localName")(si)
        val tag = v
        System.err.println("* Warning: outerHTML may returns incorrect strings.")
        val s =
          tag |> strval |> gamma_str match {
            case Some(strs) => (ValueBot /: strs)((s_i, s) => joinValue(s_i, toAbsStr(s"<$s></$s>") |> StoValue))
            case _ => StrTop |> StoValue
          }
        val ess = exception(es)(si.state)
        returnValueD(s, ess)(si)
      }),
      "Element.prototype.querySelectorAll" -> querySelectorAll,
      "Element.prototype.attributes$get" -> returnInternalProp(ii("Attributes")),
      "Element.prototype.hasAttribute" -> genAPI(si => {
        val names = callToString(si)(si.getArg(0))
        val (b, es) =
          ((ValueBot, AExceptionBot) /: names)((ve_i, name) => {
            name |> gamma_str match {
              case Some(cnames) =>
                (ve_i /: cnames)((ve_i2, cname) => {
                  val (v, es) = getDPropOfThisD(cname, ValueBot)(si)
                  val b =
                    if (v |> isBottom) BoolFalse |> BtoValue
                    else BoolTrue |> BtoValue
                  (joinValue(ve_i2._1, b), ve_i2._2 ++ es)
                })
              case _ =>
                System.err.println(s"* Info: Imprecise hasAttribute($name)")
                val (_, b_notok) = getDNodes(si.state, si.lset_this)
                val es = if (b_notok) AExceptionBot + AError else AExceptionBot

                (joinValue(ve_i._1, BoolTop |> BtoValue), ve_i._2 ++ es)
            }
          })

        val ess = exception(es)(si.state)
        returnValueD(b, ess)(si)
      }),
      "Element.prototype.getAttribute" -> genAPI(si => {
        val names = callToString(si)(si.getArg(0))
        val (v, es) =
          ((ValueBot, AExceptionBot) /: names)((ve_i, name) => {
            name |> gamma_str match {
              case Some(cnames) =>
                (ve_i /: cnames)((ve_i2, cname) => {
                  val (v, es) = getDPropOfThisD(cname, AAbsValueNull)(si)
                  (joinValue(ve_i2._1, v), ve_i2._2 ++ es)
                })
              case _ =>
                System.err.println(s"* Info: Imprecise getAttribute($name)")
                val lset_this = si.lset_this
                val (dlset_ok, b_notok) = getDNodes(si.state, lset_this)

                val v_n = (AAbsValueNull /: dlset_ok)((v_i, dl) => joinValue(v_i, StrTop |> StoValue))
                val es = if (b_notok) AExceptionBot + AError else AExceptionBot

                (joinValue(ve_i._1, v_n), ve_i._2 ++ es)
            }
          })
        val ess = exception(es)(si.state)

        returnValueD(v, ess)(si)
      }),
      "Element.prototype.setAttribute" -> genAPI(si => {
        val anames = callToString(si)(si.getArg(0))
        val value = si.getArg(1)
        val s_value = makeString(value)(si)

        val s_n =
          (StateBot /: anames)((s_i, aname) => {
            aname |> gamma_str match {
              case Some(names) =>
                (s_i /: names)((s_i2, name) => {
                  if (name.equalsIgnoreCase("className")) {
                    joinState(s_i2, assignDPropValue("@className", s_value)(si)._1)
                  } else {
                    joinState(s_i2, assignDPropValue(name, s_value)(si)._1)
                  }
                })
              case _ => throw new InternalError("TODO")
            }
          })

        returnUndef(si.copyi(state = s_n))
      }),
      "Element.prototype.removeAttribute" -> genAPI(si => {
        val anames = callToString(si)(si.getArg(0))

        val s_n =
          (StateBot /: anames)((s_i, aname) => {
            aname |> gamma_str match {
              case Some(names) =>
                (s_i /: names)((s_i2, name) => {
                  joinState(s_i2, removeDPropValue(name)(si)._1)
                })
              case _ => throw new InternalError("TODO")
            }
          })

        returnUndef(si.copyi(state = s_n))
      }),

      "Node.prototype.cloneNode" -> genAPI(si => {
        // TODO cloneNode does not clone the node indeed. :)
        val deep = si.getArg(0) |> toBoolean
        System.err.println(s"* Warning: cloneNode with $deep. We do not clone the node, and return the this node.")
        returnValueC(si.lset_this |> LStoValue)(si)
      }),
      "Node.prototype.contains" -> returnValueAPI(BoolTop |> BtoValue),
      "Node.prototype.nodeType$get" -> returnDOMProp("nodeType"),
      "Node.prototype.nodeName$get" -> returnDOMProp("nodeName"),
      "Node.prototype.firstChild$get" -> returnDOMPropElem("firstChild"),
      "Node.prototype.nextSibling$get" -> returnDOMPropElem("nextSibling"),
      "Node.prototype.parentNode$get" -> returnDOMPropElem("parentNode"),
      "Node.prototype.ownerDocument$get" -> returnDOMProp("ownerDocument"),
      "Node.prototype.lastChild$get" -> genAPI(si => {
        val ctxs = si.lset_this
        val lastChilds = lsetFoldLeftL(ctxs)(ValueBot)((v_i, ctx) => joinValue(v_i, lastChild(ctx)(si.state)))

        returnValueC(lastChilds)(si)
      }),
      "Node.prototype.childNodes$get" -> returnInternalProp(ii("ChildNodes")),
      "Node.prototype.insertBefore" -> genAPI(si => {
        val arg = si.getArg(0)
        val befores = si.getArg(1)
        val ctxs = si.lset_this
        val es =
          if (!(arg |> pvalue |> isPBottom)) AExceptionBot + ATypeError
          else AExceptionBot
        val nnodes = arg

        val s_n =
          lsetFoldLeftL(ctxs)(StateBot)((s_i, ctx) => {
            lsetFoldLeft(nnodes)(s_i)((s_i2, nnode) => {
              lsetFoldLeft(befores)(s_i2)((s_i3, before) => joinState(s_i3, insertBefore(ctx, nnode, before)(si.state)))
            })
          })

        val ess = exception(es)(si.state)
        returnValueD(nnodes, ess)(si.copyi(state = s_n))
      }),
      "Node.prototype.appendChild" -> genAPI(si => {
        val arg = si.getArg(0)
        val ctxs = si.lset_this
        val es =
          if (!(arg |> pvalue |> isPBottom)) AExceptionBot + ATypeError
          else AExceptionBot
        val childs = arg
        val s_n =
          lsetFoldLeftL(ctxs)(StateBot)((s_i, ctx) => {
            lsetFoldLeft(childs)(s_i)((s_i2, child) => joinState(s_i, appendChildA(ctx, child)(si.state)))
          })

        val ess = exception(es)(si.state)
        returnValueD(childs, ess)(si.copyi(state = s_n))
      }),
      "Node.prototype.removeChild" -> genAPI(si => {
        val arg = si.getArg(0)
        val ctxs = si.lset_this
        val es =
          if (!(arg |> pvalue |> isPBottom)) AExceptionBot + ATypeError
          else AExceptionBot
        val childs = arg
        val s_n =
          lsetFoldLeftL(ctxs)(StateBot)((s_i, ctx) => {
            lsetFoldLeft(childs)(s_i)((s_i2, child) => joinState(s_i, removeChild(ctx, child)(si.state)))
          })

        val ess = exception(es)(si.state)
        returnValueD(childs, ess)(si.copyi(state = s_n))
      }),

      "HTMLCollection.prototype.length$get" -> returnInternalProp(ii("Length")),
      "NodeList.prototype.length$get" -> returnInternalProp(ii("Length")),

      "HTMLElement.prototype.oninput$set" -> DOM2EventAdd("input"),
      "HTMLElement.prototype.oninput$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("input"),
      "HTMLElement.prototype.onclick$set" -> DOM2EventAdd("click"),
      "HTMLElement.prototype.onclick$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("click"),
      "HTMLElement.prototype.ondblclick$set" -> DOM2EventAdd("dblclick"),
      "HTMLElement.prototype.ondblclick$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("dblclick"),
      "HTMLElement.prototype.onselect$set" -> DOM2EventAdd("select"),
      "HTMLElement.prototype.onselect$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("select"),
      "HTMLElement.prototype.onselectstart$set" -> DOM2EventAdd("selectstart"),
      "HTMLElement.prototype.onselectstart$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("selectstart"),
      "HTMLElement.prototype.onkeydown$set" -> DOM2EventAdd("keydown"),
      "HTMLElement.prototype.onkeydown$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("keydown"),
      "HTMLElement.prototype.onkeypress$set" -> DOM2EventAdd("keypress"),
      "HTMLElement.prototype.onkeypress$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("keypress"),
      "HTMLElement.prototype.onpaste$set" -> DOM2EventAdd("paste"),
      "HTMLElement.prototype.onpaste$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("paste"),
      "HTMLElement.prototype.onmousedown$set" -> DOM2EventAdd("mousedown"),
      "HTMLElement.prototype.onmousedown$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("mousedown"),
      "HTMLElement.prototype.onmousemove$set" -> DOM2EventAdd("mousemove"),
      "HTMLElement.prototype.onmousemove$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("mousemove"),
      "HTMLElement.prototype.onmouseup$set" -> DOM2EventAdd("mouseup"),
      "HTMLElement.prototype.onmouseup$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("mouseup"),
      "HTMLElement.prototype.onmousewheel$set" -> DOM2EventAdd("mousewheel"),
      "HTMLElement.prototype.onmousewheel$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("mousewheel"),
      "HTMLElement.prototype.onload$set" -> DOM2EventAdd("onload"),
      "HTMLElement.prototype.onload$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("mousewheel"),
      "HTMLElement.prototype.onerror$set" -> DOM2EventAdd("onerror"),
      "HTMLElement.prototype.onerror$get" -> returnValueAPI(AAbsValueNull), //returnDOMProp("mousewheel"),
      "HTMLElement.prototype.style$set" -> warning("Ignored attempts to update the value of 'style' property"),
      "HTMLElement.prototype.style$get" -> genAPI(si => {
        val (v, es) = getIProp(ii("Style"))(si)
        val ess = exception(es)(si.state)
        returnValueD(v, ess)(si)
      }),
      "HTMLElement.prototype.offsetWidth$get" -> returnDOMProp("offsetWidth"),
      "HTMLElement.prototype.offsetHeight$get" -> returnDOMProp("offsetHeight"),
      "HTMLElement.prototype.title$get" -> returnDOMProp("title"),
      "HTMLElement.prototype.title$set" -> genAPI(si => { assignDProp("title", makeString)(si) }),

      "HTMLDocument.prototype.fgColor$set" -> genAPI(si => { assignStringDProp("fgColor")(si) }),
      "HTMLDocument.prototype.fgColor$get" -> returnDOMProp("fgColor"),

      "HTMLScriptElement.prototype.text$set" -> genAPI(si => assignStringDProp("text")(si)),
      "HTMLScriptElement.prototype.text$get" -> returnDOMProp("text"),
      "HTMLScriptElement.prototype.type$set" -> genAPI(si => { assignStringDProp("type")(si) }),
      "HTMLScriptElement.prototype.type$get" -> returnDOMProp("type"),
      "HTMLScriptElement.prototype.src$set" -> genAPI(si => {
        val name = callToString(si)(si.getArg(0)) //.filter(p => AbsBool.BoolTrue <= p.startsWith("http:"))
        if (name.nonEmpty) {
          System.out.println(s"* Warning: the given program dynamically loads a script from ${name.mkString(", ")}")
        }
        assignStringDProp("src")(si)
      }),
      "HTMLScriptElement.prototype.src$get" -> returnDOMProp("src"),

      "HTMLSelectElement.prototype.disabled$set" -> genAPI(si => { assignDProp("disabled", makeBool)(si) }),
      "HTMLSelectElement.prototype.disabled$get" -> returnDOMProp("disabled"),

      "HTMLImageElement.prototype.src$set" -> genAPI(si => {
        assignStringDProp("src")(si)
      }),
      "HTMLImageElement.prototype.src$get" -> returnDOMProp("src"),

      "HTMLIFrameElement.prototype.width$set" -> genAPI(si => { assignDProp("width", makeString)(si) }),
      "HTMLIFrameElement.prototype.height$set" -> genAPI(si => { assignDProp("height", makeString)(si) }),
      "HTMLIFrameElement.prototype.frameBorder$set" -> genAPI(si => { assignDProp("frameBorder", makeString)(si) }),
      "HTMLIFrameElement.prototype.contentWindow$get" -> genAPI(si => { returnValueC(GlobalLoc |> LtoValue)(si) }),
      "HTMLIFrameElement.prototype.contentDocument$get" -> genAPI(si => { returnValueC(getNamedNode(DocumentNode))(si) }),
      "HTMLIFrameElement.prototype.src$set" -> genAPI(si => assignStringDProp("src")(si)),
      "HTMLIFrameElement.prototype.src$get" -> returnDOMProp("src"),

      "HTMLInputElement.prototype.value$set" -> genAPI(si => { assignDProp("value", makeString)(si) }),
      "HTMLInputElement.prototype.value$get" -> returnDOMProp("value"),
      "HTMLInputElement.prototype.checked$set" -> genAPI(si => { assignDProp("checked", makeBool)(si) }),
      "HTMLInputElement.prototype.checked$get" -> returnDOMProp("checked"),
      "HTMLInputElement.prototype.type$get" -> returnDOMProp("type"),
      "HTMLInputElement.prototype.type$set" -> genAPI(si => { assignDProp("type", makeString)(si) }),

      "HTMLOptionElement.prototype.selected$set" -> genAPI(si => assignDProp("selected", makeBool)(si)),
      "HTMLOptionElement.prototype.selected$get" -> returnDOMProp("selected"),
      "HTMLOptionElement.prototype.disabled$set" -> genAPI(si => assignDProp("disabled", makeBool)(si)),
      "HTMLOptionElement.prototype.disabled$get" -> returnDOMProp("disabled"),

      "HTMLTextAreaElement.prototype.value$get" -> genAPI(si => { returnValueC(StrTop |> StoValue)(si) }),

      "HTMLAnchorElement.prototype.href$set" -> genAPI(si => { assignDProp("href", makeString)(si) }),
      "HTMLAnchorElement.prototype.href$get" -> returnDOMProp("href"),
      "HTMLAnchorElement.prototype.protocol$get" -> returnDOMProp("protocol"),
      "HTMLAnchorElement.prototype.search$get" -> returnDOMProp("search"),
      "HTMLAnchorElement.prototype.hash$get" -> returnDOMProp("hash"),
      "HTMLAnchorElement.prototype.pathname$get" -> returnDOMProp("pathname"),
      "HTMLAnchorElement.prototype.host$get" -> returnDOMProp("host"),
      "HTMLAnchorElement.prototype.hostname$get" -> returnDOMProp("hostname"),
      "HTMLAnchorElement.prototype.port$get" -> returnDOMProp("port"),
      "HTMLAnchorElement.prototype.target$get" -> returnDOMProp("target"),

      "HTMLFormElement.prototype.enctype$get" -> returnDOMProp("enctype"),

      "Event.prototype.type$get" -> returnInternalProp(ii("Type")),
      "Event.prototype.bubbles$get" -> returnInternalProp(ii("Bubbles")),
      "Event.prototype.cancelable$get" -> returnInternalProp(ii("Cancelable")),
      "Event.prototype.currentTarget$get" -> returnInternalProp(ii("CurrentTarget")),
      "Event.prototype.eventPhase$get" -> returnInternalProp(ii("EventPhase")),
      "Event.prototype.srcElement$get" -> returnInternalProp(ii("SrcElement")),
      "Event.prototype.target$get" -> returnInternalProp(ii("Target")),
      "Event.prototype.defaultPrevented$get" -> returnValueAPI(BoolFalse |> BtoValue),
      "Event.prototype.returnValue$get" -> returnValueAPI(BoolTop |> BtoValue),
      "Event.prototype.timeStamp$get" -> returnValueAPI(UInt |> NtoValue),

      "UIEvent.prototype.detail$get" -> returnInternalProp(ii("Detail")),
      "UIEvent.prototype.which$get" -> returnInternalProp(ii("Which")),
      "UIEvent.prototype.view$get" -> genAPI(si => {
        returnValueC(GlobalLoc |> LtoValue)(si)
      }),

      "KeyboardEvent.prototype.keyCode$get" -> returnInternalProp(ii("KeyCode")),

      "MouseEvent.prototype.screenX$get" -> returnInternalProp(ii("ScreenX")),
      "MouseEvent.prototype.screenY$get" -> returnInternalProp(ii("ScreenY")),
      "MouseEvent.prototype.clientX$get" -> returnInternalProp(ii("ClientX")),
      "MouseEvent.prototype.clientY$get" -> returnInternalProp(ii("ClientY")),
      "MouseEvent.prototype.ctrlKey$get" -> returnInternalProp(ii("CtrlKey")),
      "MouseEvent.prototype.shiftKey$get" -> returnInternalProp(ii("ShiftKey")),
      "MouseEvent.prototype.altKey$get" -> returnInternalProp(ii("AltKey")),
      "MouseEvent.prototype.metaKey$get" -> returnInternalProp(ii("MetaKey")),
      "MouseEvent.prototype.button$get" -> returnInternalProp(ii("Button")),
      "MouseEvent.prototype.buttons$get" -> returnInternalProp(ii("Buttons")),
      "MouseEvent.prototype.relatedTarget$get" -> returnInternalProp(ii("RelatedTarget")),
      "MouseEvent.prototype.pageX$get" -> returnInternalProp(ii("PageX")),
      "MouseEvent.prototype.pageY$get" -> returnInternalProp(ii("PageY")),
      "MouseEvent.prototype.x$get" -> returnInternalProp(ii("X")),
      "MouseEvent.prototype.y$get" -> returnInternalProp(ii("Y")),
      "MouseEvent.prototype.offsetX$get" -> returnInternalProp(ii("OffsetX")),
      "MouseEvent.prototype.offsetY$get" -> returnInternalProp(ii("OffsetY")),
      "MouseEvent.prototype.movementX$get" -> returnInternalProp(ii("MovementX")),
      "MouseEvent.prototype.movementY$get" -> returnInternalProp(ii("MovementY")),
      "MouseEvent.prototype.which$get" -> returnInternalProp(ii("Which")),
      "MouseEvent.prototype.layerX$get" -> returnInternalProp(ii("LayerX")),
      "MouseEvent.prototype.layerY$get" -> returnInternalProp(ii("LayerY")),
      "MouseEvent.prototype.fromElement$get" -> returnInternalProp(ii("Target")),
      "MouseEvent.prototype.toElement$get" -> returnInternalProp(ii("Target")),

      "DocumentType.prototype.name$get" -> returnDOMProp("name"),

      "CSSStyleDeclaration.prototype.getPropertyValue" -> genAPI(si => {
        val anames: Set[AAbsStr] = callToString(si)(si.getArg(0))

        val s =
          (ValueBot /: anames)((s_i, aname) => {
            aname |> gamma_str match {
              case Some(names) =>
                (s_i /: names)((s_i2, name) => {
                  joinValue(s_i2, getPropertyValue(si.lset_this, name.toLowerCase, si.id)(si.state))
                })
              case None =>
                System.err.println(s"* Warning: TODO: getPropertyValue($aname)")
                joinValue(s_i, OtherStr |> StoValue)
            }
          })

        returnValueC(s)(si)
      }),

      "XMLHttpRequest" -> genAPI(si => {
        val l_r = si.newAlloc(10)
        val si_0 =
          si |>
            genObject(l_r, "XMLHttpRequest", sysLoc("#XMLHttpRequest.prototype"), (o: AAbsObj) => {
              o |> updatei(ii("Readystatechange"), AAbsValueNull |> toIValue) |>
                updatei(ii("ReadyState"), toAbsNum(0) |> NtoValue |> toIValue)
            })
        returnValueC(l_r |> LtoValue)(si_0)
      }),
      "XMLHttpRequest.prototype.send" -> XMLHttpRequestSend,
      "XMLHttpRequest.prototype.getResponseHeader" -> genAPI(si => { returnValueC(StrTop |> StoValue)(si) }),
      "XMLHttpRequest.prototype.responseText$get" -> genAPI(si => returnValueC(StrTop |> StoValue)(si)),
      "XMLHttpRequest.prototype.responseXML$get" -> genAPI(si => returnValueC(StrTop |> StoValue)(si)),

      "XMLHttpRequest.prototype.status$get" -> returnValueAPI(joinValue(toAbsNum(200) |> NtoValue, toAbsNum(404) |> NtoValue)),
      "XMLHttpRequest.prototype.statusText$get" -> returnValueAPI(joinValue(toAbsStr("") |> StoValue, toAbsStr("OK") |> StoValue)),
      "XMLHttpRequest.prototype.readyState$get" -> returnInternalProp(ii("ReadyState")),
      "XMLHttpRequest.prototype.open" -> genAPI(si => {
        val s_n =
          lsetFoldLeftL(si.lset_this)(StateBot)((s_i, l) => {
            val o = si.state |> slookup(l) |>
              updatei(ii("ReadyState"), toAbsNum(1) |> NtoValue |> toIValue) |>
              updatei(ii("ResponseURL"), si.getArg(1) |> toIValue)
            joinState(s_i, si.state |> supdate(l, o))
          })
        returnUndef(si.copyi(state = s_n))
      }),
      "XMLHttpRequest.prototype.setRequestHeader" -> genAPI(si => returnUndef(si)),
      "XMLHttpRequest.prototype.abort" -> genAPI(si => returnUndef(si)),
      "XMLHttpRequest.prototype.onreadystatechange$get" -> genAPI(si => {
        val v =
          lsetFoldLeftL(si.lset_this)(ValueBot)((v_i, l) => {
            joinValue(v_i, si.state |> slookup(l) |> lookupi(ii("Readystatechange")) |> ItoValue)
          })
        returnValueC(v)(si)
      }),
      "XMLHttpRequest.prototype.onreadystatechange$set" -> genAPI(si => {
        val cb = si.getArg(0)
        val lset_f = filterCallable(si.state, cb)
        // need to consider exceptional cases.
        val s_n = lsetFoldLeftL(si.lset_this)(StateBot)((s_i, l) => si.state |> supdate(l, si.state |> slookup(l) |> updatei(ii("Readystatechange"), lset_f |> toIValue)))
        val dglobal = DLoc.sys("#Global")
        val o = s_n |> dlookup(dglobal)
        val o_n = lsetFoldLeft(lset_f)(o)((o_i, l) => o_i |> addEventListener("xml_readystatechange", l, bubble = false))
        val s_n2 = s_n |> dupdate(dglobal, o_n)

        returnValueC(lset_f)(si.copyi(state = s_n2))
      }),

      "Performance.prototype.timing$get" -> genAPI(si => {
        val l_r = si.newRecentAlloc(10)
        val l_proto = sysLoc("#PerformanceTiming.prototype")
        val o = newObject(l_proto, "PerformanceTiming")
        val s = si.state |> supdate(l_r, o)
        returnValueC(l_r |> LtoValue)(si.copyi(state = s))
      }),

      "#PerformanceTiming.prototype.responseStart$get" -> returnValueAPI(UInt |> NtoValue),
      "#PerformanceTiming.prototype.navigationStart$get" -> returnValueAPI(UInt |> NtoValue),

      "Storage.prototype.getItem" -> genAPI(si => { returnValueC(AAbsValueNull)(si) }),
      "Storage.prototype.setItem" -> genAPI(si => returnUndef(si)),
      "Storage.prototype.removeItem" -> genAPI(si => returnUndef(si)),

      "Audio" -> genAPI(si => {
        val lset = si.lset_this |> LStoValue |> remove(GlobalLoc)
        val cls = "Audio"
        val l_proto = sysLoc(s"#$cls.prototype")
        val o_new = newObject(l_proto, cls) |> updatei(ii("Src"), toAbsStr("") |> StoValue |> toIValue)
        val s_n = lsetFoldLeft(lset)(si.state)((s_i, l) => s_i |> supdate(l, o_new))

        returnValueC(lset)(si.copyi(state = s_n))
      }),

      "HTMLMediaElement.prototype.play" -> genAPI(si => {
        System.err.println("Warning: play should return Promise object.")
        returnUndef(si)
      }),
      "HTMLMediaElement.prototype.src$set" -> genAPI(si => assignIProp(ii("Src"))(si)),
      "HTMLMediaElement.prototype.src$get" -> returnInternalProp(ii("Src"))
    )
    System.out.println(s"* The number of semantics: ${ssSemantics.size}") // 246
    name: String => ssSemantics.getOrElse(name.substring(1), nnAPI(name))
  }
}
