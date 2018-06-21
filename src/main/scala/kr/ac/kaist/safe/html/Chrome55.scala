/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.safe.html

import org.w3c.dom.Node

import scala.collection.immutable.HashSet
import scala.language.reflectiveCalls
import kr.ac.kaist.compabs.models.{ DLoc, _ }
import kr.ac.kaist.compabs.models.cdomain.CValue
import kr.ac.kaist.compabs.models.html.{ CodeFragment, IChrome55 }
import kr.ac.kaist.compabs.models.shape.Host.ModelError
import kr.ac.kaist.safe.BASE_DIR
import kr.ac.kaist.safe.analyzer.{ Helper, Initialize, TypeConversionHelper }
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinGlobal
import kr.ac.kaist.safe.util.NodeUtil.{ INTERNAL_BOOL_TOP, INTERNAL_CALL, INTERNAL_EVENT_FUNC }
import kr.ac.kaist.safe.util.UserAllocSite

object Chrome55 extends IChrome55 {
  override val path: String = BASE_DIR + "/src/main/resources/DOMModels/"
  override val name: String = "heap.shape"

  type AAbsState = AbsState
  type AAbsValue = AbsValue
  type AAbsPValue = AbsPValue
  type AAbsBool = AbsBool
  type AAbsStr = AbsStr
  type AAbsNum = AbsNum
  type AAbsLoc = AbsLoc
  type ALoc = Loc
  type AAbsObj = AbsObj
  type AAbsIValue = AbsIValue
  type AException = Exception
  type AAbsDataProp = AbsDataProp
  // wrapper type
  override type SemanticsFun = (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue)

  lazy val AError: Exception = Error
  lazy val ATypeError: AException = TypeError
  lazy val ADOMError: AException = Error
  lazy val AAbsValueNull: AAbsValue = AbsValue(AbsNull.Top)
  lazy val BoolTrue: AAbsBool = AbsBool.True
  lazy val BoolFalse: AAbsBool = AbsBool.False
  lazy val BoolTop: AAbsBool = AbsBool.Top
  lazy val BoolBot: AAbsBool = AbsBool.Bot
  lazy val ValueBot: AAbsValue = AbsValue.Bot
  lazy val StateBot: AAbsState = AbsState.Bot
  lazy val LocSetBot: AbsLoc = AbsLoc.Bot
  lazy val AbsObjBot: AAbsObj = AbsObj.Bot
  lazy val UInt: AbsNum = AbsNum.UInt
  lazy val IntStr: AAbsStr = AbsStr.Number
  lazy val StrTop: AAbsStr = AbsStr.Top
  lazy val OtherStr: AAbsStr = AbsStr.Other
  lazy val GlobalLoc: Loc = BuiltinGlobal.loc

  case class SFInput(args: AbsValue, st: AbsState) extends SFInputT {
    val state: AbsState = st
    def getArg(i: Int): AbsValue = Helper.propLoad(args, Set(AbsStr("0")), st.heap)
    def lenArg: AbsNum = Helper.propLoad(args, Set(AbsStr("length")), st.heap).pvalue.numval
    def newAlloc(i: Long): ALoc = UserAllocSite((id + i).toInt)
    def newRecentAlloc(i: Long): ALoc = newAlloc(i)
    def newDAlloc(i: Long): DLoc.T = DLoc.recent(id + i)

    val id: Long = 10000
    lazy val lset_this: AAbsLoc = v_this.locset
    lazy val v_this: AAbsValue = st.context.thisBinding

    override def copyi(state: AbsState): SFInput = this.copy(st = state)
  }

  // semantic wrapper
  override def genAPI(i: SFInput => (AAbsState, AAbsState, AAbsValue)): SemanticsFun = {
    (args: AbsValue, s: AbsState) => i(SFInput(args, s))
  }
  def nnAPI(name: String): SemanticsFun = genAPI(si => throw new InternalError(s"TODO: $name"))

  override def getProgram(t: T): List[CodeFragment] = {
    // Extract JavaScript code from HTML. Not event handlers.
    val codes = super.getProgram(t)

    val evt = INTERNAL_EVENT_FUNC
    // Build a CFG that includes an event handling loop.
    val event_body = CodeFragment("#event#loop", 0, 0, s"while($INTERNAL_BOOL_TOP) { $INTERNAL_CALL($evt.func, $evt.elem, []); }")
    codes :+ event_body
  }

  // utilities
  def returnValueA(v: AbsBool)(s: SFInput): (AbsState, AbsState, AbsValue) = returnValueC(AbsValue(v))(s)
  def returnValueB(v: AbsStr)(s: SFInput): (AbsState, AbsState, AbsValue) = returnValueC(AbsValue(v))(s)
  def returnValueC(v: AbsValue)(s: SFInput): (AbsState, AbsState, AbsValue) = returnValueD(v, AbsState.Bot)(s)
  def returnValueD(v: AbsValue, es: AbsState)(s: SFInput): (AbsState, AbsState, AbsValue) = (s.st, es, v)
  def returnValueAPI(v: AbsValue): SemanticsFun = genAPI(returnValueC(v))
  def returnValueAPI(b: Boolean): SemanticsFun = genAPI(returnValueC(AbsValue(AbsBool.alpha(b))))
  def returnValueAPI(s: String): SemanticsFun = genAPI(returnValueC(AbsValue(AbsStr.alpha(s))))
  def returnValueLocName(s: String): SemanticsFun = genAPI(returnValueC(AbsValue(Loc(s.substring(1)))))
  def returnValueIRange(l: Int, u: Int): SemanticsFun = {
    genAPI(returnValueC(AbsValue(AbsNum.alpha((l to u).map(_.toDouble).toSet))))
  }
  def returnUndef(si: SFInput): (AAbsState, AAbsState, AAbsValue) = returnValueC(AbsValue(Undef))(si)
  override def filterCallable(s: AAbsState, ls: AAbsValue): AAbsValue = {
    AbsValue(ls.locset.filter(l => AbsBool.True ⊑ TypeConversionHelper.IsCallable(l, s.heap)))
  }

  def isNonEmpty(v: AAbsValue): Boolean = !v.locset.isBottom
  def newAddr(): ALoc = UserAllocSite(0)
  def toPBot(v: AAbsValue): AAbsValue = v.copy(pvalue = AbsPValue.Bot)
  def filter(v: AAbsValue): (AAbsValue, AAbsValue) = {
    val v_1 = v.copy(pvalue = v.pvalue.copy(undefval = AbsUndef.Bot, nullval = AbsNull.Bot))
    val v_2 = AbsValue(v.pvalue.nullval) ⊔ AbsValue(v.pvalue.undefval)

    (v_1, v_2)
  }
  def toDOMString(v: AAbsValue): HashSet[String] = {
    val empty = HashSet.empty[String]
    val s_1 = if (v.pvalue.nullval.isTop) empty + null else empty
    val str = TypeConversionHelper.ToPrimitive(v).toStringSet

    val strs =
      (HashSet.empty[Str] /: str)((s_i, s) => {
        s.gamma match {
          case ConFin(ss) =>
            s_i ++ ss
          case _ => throw new InternalError("TODO")
        }
      })

    (s_1 /: strs) {
      case (s_i, Str(s)) => s_i + s
    }
  }

  override def exception(es: Set[AException])(s: AAbsState): AAbsState = {
    if (es.isEmpty) AbsState.Bot
    else s.raiseException(es)
  }

  def callToString(s: SFInput)(v: AAbsValue): Set[AAbsStr] = {
    TypeConversionHelper.ToPrimitive(v, s.st.heap).toStringSet
  }

  def gamma_str(s: AbsStr): Option[Set[String]] = {
    s.gamma match {
      case ConFin(strs) => Some(strs.map(p => p.str))
      case _ => None
    }
  }

  def oldify(l: ALoc)(s: AAbsState): AAbsState = s.oldify(l)
  def doldify(l: Long)(s: AAbsState): AAbsState = s.doldify(l)
  def sysLoc(name: String): ALoc = Loc.apply(name.substring(1))
  def isSingleton(l: ALoc): Boolean = l.isSingleton
  def isSingleStr(s: AAbsStr): Boolean = s.getSingle match { case ConOne(_) => true case _ => false }
  def remove(l: ALoc)(v: AAbsValue): AAbsValue = AbsValue(v.locset - l)
  def isSingleLoc(v: AAbsValue): Boolean = v.locset.isConcrete
  def getSingleLoc(v: AAbsValue): ALoc = v.locset.foldLeft(List.empty[ALoc])((l_i, l) => l :: l_i).head

  def newObject(l_proto: Loc, cls: String, bExt: AAbsBool): AAbsObj = AbsObj.newObject(l_proto)
  lazy val emptyObject: AAbsObj = AbsObj.newObject
  def newArrayObject(v: AAbsNum): AAbsObj = AbsObj.newArrayObject(v)
  def newFuncObject: AAbsObj = AbsObj.newObject
  def toAbsNum(i: Double): AAbsNum = AbsNum.alpha(Num(i))

  def updateIProp(name: String, value: CValue)(od: (AAbsObj, DNode.T)): (AAbsObj, DNode.T) = {
    (od._1, od._2.update(name, AbsValue.alpha(Initialize.convToV(value))))
  }

  def updateProp(name: String, value: CValue)(od: (AAbsObj, DNode.T)): (AAbsObj, DNode.T) = {
    (od._1.update(name, AbsDataProp(AbsValue.alpha(Initialize.convToV(value)), BoolTrue, BoolFalse, BoolTrue)), od._2)
  }

  def updateProps(name: String, node: Node, v_doc: AAbsValue)(o: AAbsObj, d: DNode.T): (AAbsObj, DNode.T) = {
    val nattr: String => String = attr(node)
    name match {
      case "Document" =>
        updateProps("Node", node, v_doc)(o, d) |>
          updateIProp("@defaultView", CValue.loc("#Global")) |>
          updateIProp("cookie", CValue("")) |>
          updateIProp("readyState", CValue("loading"))

      case "Object" => (o, d)
      case "Attr" =>
        // @Node.prototype
        updateProps("Node", node, v_doc)(o, d) |>
          // namespaceURI
          updateProp("namespaceURI", CValue(node.getNamespaceURI)) |>
          // textContent
          updateProp("textContent", CValue(node.getTextContent)) |>
          // localName
          updateIProp("localName", CValue(node.getLocalName)) |>
          // name
          updateIProp("name", CValue(nattr("name"))) |>
          // prefix
          updateProp("prefix", CValue(node.getPrefix))
      // specified
      // ownerElement
      // value
      case "EventTarget" =>
        // @Object
        updateProps("Object", node, v_doc)(o, d)

      case "Node" =>
        // @EventTarget
        updateProps("EventTarget", node, v_doc)(o, d) |>
          // nodeValue
          // textContent
          // nodeName
          // parentElement
          // baseURI
          // ownerDocument
          // parentNode
          // nextSibling
          // firstChild
          // lastChild
          // childNodes
          // nodeType
          // previousSibling
          updateProp("nodeValue", CValue(node.getNodeValue)) |>
          updateProp("textContent", CValue(node.getTextContent)) |>
          updateIProp("nodeName", CValue(node.getNodeName)) |>
          updateIProp("nodeType", CValue(node.getNodeType)) |>
          updateProp("baseURI", CValue(node.getBaseURI)) |>
          updateIProp("ownerDocument", v_doc)

      case "Text" =>
        // @CharacterData
        updateProps("CharacterData", node, v_doc)(o, d)
      // wholeText

      case "CharacterData" =>
        // @Node
        updateProps("Node", node, v_doc)(o, d)
      // length
      // nextElementSibling
      // data
      // previousElementSibling

      case "CDATASection" =>
        // @Text
        updateProps("Text", node, v_doc)(o, d)

      case "Comment" =>
        // @CharacterData
        updateProps("CharacterData", node, v_doc)(o, d)

      case "DocumentFragment" =>
        // @Node
        updateProps("Node", node, v_doc)(o, d)
      // childElementCount
      // children
      // firstElementChild
      // lastElementChild

      case "DocumentType" =>
        // @Node
        updateProps("Node", node, v_doc)(o, d) |>
          // name
          updateIProp("name", CValue(nattr("name")))
      // systemId
      // publicId

      case "HTMLAnchorElement" =>
        // @HTMLElement
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          // name
          updateIProp("name", CValue(nattr("name"))) |>
          // protocol
          updateIProp("protocol", CValue(nattr("protocol"))) |>
          // search
          updateIProp("search", CValue(nattr("search"))) |>
          // hash
          updateIProp("hash", CValue(nattr("hash"))) |>
          // pathname
          updateIProp("pathname", CValue(nattr("pathname"))) |>
          // host
          updateIProp("host", CValue(nattr("host"))) |>
          // hostname
          updateIProp("hostname", CValue(nattr("hostname"))) |>
          // port
          updateIProp("port", CValue(nattr("port"))) |>
          // href
          updateIProp("href", CValue(nattr("href"))) |>
          // target
          updateIProp("target", CValue(nattr("target")))
      // origin
      // username
      // download
      // password
      // rel
      // ping
      // charset
      // rev
      // text
      // coords
      // hreflang
      // type
      // shape

      case "HTMLAppletElement" =>
        // @HTMLElement
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          // name
          updateIProp("name", CValue(nattr("name")))
      // width
      // heigth

      case "HTMLAreaElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLBaseElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)
      // target
      // href

      case "HTMLBaseFontElement" => (o, d)
      case "HTMLBodyElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)
      // onfocus
      // onbeforeunload
      // onscroll
      // background
      // onerror
      // onblur
      // onmessage
      // link
      // bgColor
      // onstorage
      // onpageshow
      // onpagehide
      // ononline
      // onload
      // onresize
      // onunload
      // vLink
      // onlanguagechange
      // text
      // aLink
      // onpopstate
      // onhashchange
      // onoffline

      case "HTMLBRElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)
      // clear

      case "HTMLButtonElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          // name
          updateIProp("name", CValue(nattr("name")))
      // validationMessage
      // disabled
      // formEnctype
      // autofocus
      // form
      // formTarget
      // formNoValidate
      // willValidate
      // value
      // validity
      // formAction
      // formMethod
      // labels
      // type

      case "HTMLCollection" =>
        updateProps("Object", node, v_doc)(o, d)
      // length

      case "HTMLDirectoryElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)
      // compact

      case "HTMLDivElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)
      // align

      case "HTMLDListElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)
      // compact

      case "HTMLDocument" =>
        updateProps("Document", node, v_doc)(o, d) |>
          // all
          // alinkColor
          // fgColor
          // bgColor
          // linkColor
          // vlinkColor
          updateIProp("fgColor", CValue(""))
      case "HTMLDOMImplementation" => (o, d)
      case "HTMLFieldSetElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          // name
          updateIProp("name", CValue(nattr("name")))
      // validationMessage
      // disabled
      // form
      // elements
      // willValidate
      // validity
      // type

      case "HTMLFontElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)
      // size
      // color
      // face

      case "HTMLFormElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          // name
          updateIProp("name", CValue(nattr("name"))) |>
          // enctype
          updateIProp("enctype", CValue("application/x-www-form-urlencoded"))
      // length
      // noValidate
      // acceptCharset
      // method
      // elements
      // encoding
      // autocomplete
      // target
      // action

      case "HTMLFrameElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLFrameSetElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLHeadElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLHeadingElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLHRElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLHtmlElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLIFrameElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          updateIProp("src", CValue(nattr("src"))) |>
          updateIProp("width", CValue(nattr("width"))) |>
          updateIProp("height", CValue(nattr("heigth"))) |>
          updateIProp("frameBorder", CValue(nattr("frameBorder"))) |>
          updateIProp("contentWindow", CValue(nattr("contentWindow"))) |>
          updateIProp("contentDocument", CValue(nattr("contentDocument")))

      case "HTMLImageElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          // name
          updateIProp("name", CValue(nattr("name"))) |>
          updateIProp("src", CValue(nattr("src")))
      // y
      // border
      // align
      // src
      // alt
      // x
      // width
      // hspace
      // height
      // vspace
      // complete

      case "HTMLInputElement" =>
        val vtype = nattr("type")

        vtype.toLowerCase() match {
          case "checkbox" =>
            updateProps("HTMLElement", node, v_doc)(o, d) |>
              updateIProp("value", AbsValue.alpha(Str("on"))) |>
              updateIProp("type", AbsValue.alpha(Str(vtype)))
          case "radio" =>
            updateProps("HTMLElement", node, v_doc)(o, d) |>
              updateIProp("value", AbsValue.alpha(Str("on"))) |>
              updateIProp("checked", AbsValue.alpha(Bool(true))) |>
              updateIProp("type", AbsValue.alpha(Str(vtype)))
          case "text" | "" | "password" | "hidden" | "button" | "search" =>
            updateProps("HTMLElement", node, v_doc)(o, d) |>
              updateIProp("value", AbsValue.alpha(Str(nattr("value")))) |>
              updateIProp("type", AbsValue.alpha(Str(vtype)))
          case "submit" =>
            updateProps("HTMLElement", node, v_doc)(o, d) |>
              updateIProp("value", AbsValue.alpha(Str(nattr("value")))) |>
              updateIProp("type", AbsValue.alpha(Str(vtype)))
          case s => throw new ModelError(s"TODO: $s")
        }

      case "HTMLIsIndexElement" => (o, d)
      case "HTMLLabelElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLLegendElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLLIElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLLinkElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLMapElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLMenuElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLMetaElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLModElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLObjectElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLOListElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLOptGroupElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLOptionElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          updateIProp("selected", AbsValue.alpha(Bool(false))) |>
          updateIProp("disabled", AbsValue.alpha(Bool(false)))

      case "HTMLParagraphElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLParamElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLPreElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLQuoteElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLScriptElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLSelectElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d) |>
          updateIProp("@selected", AbsValue(DLoc.nullv)) |>
          updateIProp("disabled", AbsValue(BoolFalse))

      case "HTMLStyleElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTableCaptionElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTableCellElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTableColElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTableElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTableRowElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTableSectionElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTextAreaElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLTitleElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLUListElement" =>
        updateProps("HTMLElement", node, v_doc)(o, d)

      case "HTMLElement" =>
        // @Element
        updateProps("Element", node, v_doc)(o, d) |>
          updateIProp("title", AbsValue.alpha(Str(nattr("title"))))

      // onfocus
      // onscroll
      // contentEditable
      // onautocomplete
      // ontoggle
      // onmouseup
      // onstalled
      // onselect
      // onplay
      // onreset
      // innerText
      // onmousedown
      // onprogress
      // ondragover
      // onerror
      // dir
      // onseeked
      // onblur
      // onmouseleave
      // oncuechange
      // onloadstart
      // onwaiting
      // ondrop
      // ondurationchange
      // onmousemove
      // ondragleave
      // onmousewheel
      // offsetWidth
      // offsetParent
      // oncontextmenu
      // onabort
      // ondragend
      // offsetTop
      // oninvalid
      // onended
      // onsuspend
      // outerText
      // webkitdropzone
      // onmouseover
      // ondrag
      // onkeypress
      // hidden
      // onemptied
      // onshow
      // onratechange
      // onseeking
      // style
      // onkeydown
      // isContentEditable
      // oncancel
      // oncanplay
      // offsetLeft
      // ondblclick
      // onloadeddata
      // title
      // onplaying
      // onclose
      // onsubmit
      // ondragstart
      // oncanplaythrough
      // dataset
      // accessKey
      // onvolumechange
      // onloadedmetadata
      // ondragenter
      // lang
      // tabIndex
      // onkeyup
      // translate
      // onload
      // onclick
      // onresize
      // draggable
      // onchange
      // onautocompleteerror
      // onpause
      // offsetHeight
      // oninput
      // onmouseenter
      // ontimeupdate
      // spellcheck
      // onmouseout

      case "Element" =>
        // @Node
        updateProps("Node", node, v_doc)(o, d) |>
          // onbeforepaste
          // namespaceURI
          // onbeforecut
          // onbeforecopy
          // onpaste
          // nextElementSibling
          // attributes
          // childElementCount
          // offsetLeft
          // onwebkitfullscreenchange
          // onwheel
          // oncopy
          // onsearch
          // tagName
          // previousElementSibling
          // children
          // classList
          // firstElementChild
          // offsetHeight
          // prefix
          // onselectstart
          // lastElementChild
          // onwebkitfullscreenerror
          // oncut
          // shadowRoot
          // clientWidth
          // offsetWidth
          // offsetParent
          // offsetTop
          // clientHeight
          // className
          updateIProp("id", CValue(nattr("id"))) |>
          updateIProp("className", CValue(nattr("class"))) |>
          // localName
          updateIProp("localName", CValue(node.getNodeName.toLowerCase)) |>
          /** see: https://drafts.csswg.org/cssom-view/#dom-element-clientleft **/
          updateIProp("offsetWidth", CValue.unknownNumber) |>
          updateIProp("offsetHeight", CValue.unknownNumber) |>
          updateIProp("clientTop", CValue.unknownNumber) |>
          updateIProp("clientLeft", CValue.unknownNumber) |>
          updateIProp("clientWidth", CValue.unknownNumber) |>
          updateIProp("clientHeight", CValue.unknownNumber) |>
          updateIProp("scrollTop", CValue.unknownNumber) |>
          updateIProp("scrollLeft", CValue.unknownNumber) |>
          updateIProp("scrollWidth", CValue.unknownNumber) |>
          updateIProp("scrollHeight", CValue.unknownNumber) |>
          (od => (od /: attrs(node))((od_i, kv) => updateIProp(kv._1, CValue(kv._2))(od_i)))
      /** see: https://www.w3.org/TR/DOM-Parsing/#the-domparser-interface **/
      // innerHTML
      // outerHTML
      case _ =>
        throw new ModelError(s"Missing cases for tag element '$name'")
    }
  }

  def toDataProp(v: AAbsValue): AAbsDataProp = AbsDataProp(v)
  def toDataProp(v: AAbsValue, w: AAbsBool, e: AAbsBool, c: AAbsBool): AAbsDataProp = AbsDataProp(v, w, e, c)
  def update(name: IName, v: AAbsIValue)(o: AAbsObj): AAbsObj = o.update(name, v)
  def update(name: String, v: AAbsDataProp)(o: AAbsObj): AAbsObj = o.update(name, v)
  def update(name: AbsStr, v: AAbsDataProp)(o: AAbsObj): AAbsObj = o.weakUpdate(name, v)
  def supdate(l: ALoc, o: AAbsObj)(s: AAbsState): AAbsState = s.copy(heap = s.heap.update(l, o))
  def slookup(l: ALoc)(s: AAbsState): AAbsObj = s.heap.get(l)
  def lookupi(n: IName)(o: AAbsObj): AAbsIValue = o(n)
  def lookup(s: AAbsStr)(o: AAbsObj): AAbsValue = o(s).value
  def dupdate(name: String, v: AAbsValue)(o: DNode.T): DNode.T = o.update(name, v)
  def dlookup(l: DLoc.T)(s: AAbsState): DNode.T = s.dom.lookup(l)
  def ddlookup(s: String)(o: DNode.T): AAbsValue = o.lookup(s)
  def ddlookupD(s: String, v: AAbsValue)(o: DNode.T): AAbsValue = o.lookupD(s, v)
  def toAbsStr(s: String): AAbsStr = AbsStr(s)
  def addEventListener(cname: String, h: ALoc, bubble: Boolean)(o: DNode.T): DNode.T = o.addEventListener(cname, h, bubble)
  def removeEventListener(cname: String, h: ALoc, bubble: Boolean)(o: DNode.T): DNode.T = o.removeEventListener(cname, h, bubble)
  def joinObj(v1: AAbsObj, v2: AAbsObj): AAbsObj = v1 ⊔ v2
  def joinValue(v1: AAbsValue, v2: AAbsValue): AAbsValue = v1 ⊔ v2
  def joinIValue(v1: AAbsIValue, v2: AAbsIValue): AAbsIValue = v1 ⊔ v2
  def joinState(v1: AAbsState, v2: AAbsState): AAbsState = v1 ⊔ v2
  def joinLocSet(v1: AAbsLoc, v2: AAbsLoc): AAbsLoc = v1 ⊔ v2
  def joinBool(v1: AAbsBool, v2: AAbsBool): AAbsBool = v1 ⊔ v2

  def NgetSingle(v: AAbsValue): Option[Double] = v.pvalue.numval.getSingle match {
    case ConOne(i) => Some(i)
    case _ => None
  }

  def isRelated(s: String)(v: AAbsStr): Boolean = v isRelated s
  def isARelated(s: AAbsStr)(v: AAbsStr): Boolean = v isRelated s
  def sdomIn(l: ALoc)(s: AAbsState): Boolean = s.heap.domIn(l)
  def domIni(l: IName)(o: AAbsObj): AbsBool = o contains l
  def has(b: Boolean)(a: AAbsBool): Boolean = AbsBool.alpha(b) ⊑ a
  def orderBool(b0: AAbsBool, b1: AAbsBool): Boolean = b0 ⊑ b1
  def dupdate(dl: DLoc.T, od: DNode.T)(s: AbsState): AbsState = s.copy(dom = s.dom.update(dl, od))
  def isBottom(v: AAbsValue): Boolean = v.isBottom
  def isPBottom(v: AAbsPValue): Boolean = v.isBottom
  def isBBottom(v: AAbsBool): Boolean = v.isBottom
  def hasNull(v: AAbsValue): Boolean = v.pvalue.nullval !⊑ AbsNull.Bot
  def isNonZero(v: AAbsNum): Boolean = orderBool(BoolFalse, v.StrictEquals(AbsNum.alpha(Num(0))))
  def strval(v: AAbsValue): AAbsStr = v.pvalue.strval
  def pvalue(v: AAbsValue): AAbsPValue = v.pvalue
  def StrictEquals(v: AAbsStr)(v2: AAbsStr): AAbsBool = v.StrictEquals(v2)

  def toBoolean(v: AAbsValue): AAbsBool = TypeConversionHelper.ToBoolean(v)

  def toIValue(dl: DLoc.T): AAbsIValue = AbsIValue(AbsValue(DLocSet.bottom + dl))
  def toIValue(l: AbsValue): AbsIValue = AbsIValue(l)

  def DtoValue(dl: DLoc.T): AAbsValue = AbsValue(dl)
  def DStoValue(dl: DLocSet.T): AAbsValue = AbsValue(dl)
  def LtoValue(l: ALoc): AAbsValue = AbsValue(l)
  def LStoValue(l: AbsLoc): AAbsValue = AbsValue(l)
  def StoValue(s: AAbsStr): AAbsValue = AbsValue(s)
  def NtoValue(n: AbsNum): AbsValue = AbsValue(n)
  def ItoValue(i: AAbsIValue): AAbsValue = i.value
  def BtoValue(i: AAbsBool): AAbsValue = AbsValue(i)

  def dlocset(v: AAbsValue): DLocSet.T = v.dlocset
  def locset(v: AAbsValue): AbsLoc = v.locset

  def lsetFoldLeft[T](v: AAbsValue)(i: T)(f: (T, ALoc) => T): T = v.locset.foldLeft[T](i)(f)
  def lsetFoldLeftL[T](v: AAbsLoc)(i: T)(f: (T, ALoc) => T): T = v.foldLeft[T](i)(f)

  def init(t: T, s: AAbsState): AAbsState = {
    val document = toDOM(t)
    def span(n: Node): Option[(String, Int, Int)] = None
    // TODO parse a given code and create a new function
    def ff(s: String, o: Option[(String, Int, Int)]): AAbsObj = newFuncObject
    val pf: (String, Option[(String, Int, Int)]) => AAbsObj = ff // Helper.parseFunction(t.js.cfg)
    genInitialDOMTree(document, span, s, pf)
  }

  // TODO
  override def XMLHttpRequestSend: SemanticsFun = genAPI(si => throw new InternalError("TODO"))
}
