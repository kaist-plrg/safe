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

package kr.ac.kaist.safe.analyzer

import scala.util.{ Try, Failure => Fail, Success => Succ }
import kr.ac.kaist.safe.parser.{ Parser => JSParser }
import kr.ac.kaist.compabs.models.cdomain._
import kr.ac.kaist.compabs.models.html.{ DOMModel, ModelCode }
import kr.ac.kaist.compabs.models.shape._
import kr.ac.kaist.safe.{ CmdCFGBuild, SafeConfig }
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.analyzer.models.builtin._
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.phase._

import scala.collection.immutable.{ HashMap, HashSet }

object Initialize {
  def apply(cfg: CFG, jsModel: Boolean, host: Option[DOMModel] = None): AbsState = {
    val globalLocSet = AbsLoc(BuiltinGlobal.loc)
    val globalPureLocalEnv = AbsLexEnv.newPureLocal(globalLocSet)
    val initHeap = AbsHeap(HashMap(
      BuiltinGlobal.loc -> AbsObj.Bot
    // TODO If delete, not working because not allowed update to bottom heap
    ))

    val initCtx = AbsContext(HashMap[Loc, AbsLexEnv](
      PredAllocSite.GLOBAL_ENV -> AbsLexEnv(AbsGlobalEnvRec.Top),
      PredAllocSite.PURE_LOCAL -> globalPureLocalEnv,
      PredAllocSite.COLLAPSED -> AbsLexEnv(AbsDecEnvRec.Empty)
    ), OldASiteSet.Empty, globalLocSet)

    val modeledHeap: AbsHeap =
      host match {
        case Some(h) =>
          val model = loadHostEnv(cfg, h)

          model.funcs.foreach {
            case (_, func) => cfg.addJSModel(func)
          }
          AbsHeap(model.heap)
        case None =>
          if (jsModel) {
            val model = HeapBuild.jscache getOrElse {
              // val fileName = NodeUtil.jsModelsBase + "snapshot_and_built_in.jsmodel"
              // ModelParser.parseFile(fileName).get
              ModelParser.mergeJsModels(NodeUtil.jsModelsBase)
            }

            model.funcs.foreach {
              case (_, func) => cfg.addJSModel(func)
            }

            AbsHeap(model.heap)
          } else {
            // TODO loads a host model
            BuiltinGlobal.initHeap(initHeap, cfg)
          }
      }

    AbsState.Bot.copy(heap = modeledHeap, context = initCtx)
  }

  private val IValueMap = HashMap(
    "@class" -> IClass,
    "@primitive" -> IPrimitiveValue,
    "@extensible" -> IExtensible,
    "@proto" -> IPrototype,
    "@function" -> ICall,
    "@construct" -> IConstruct,
    "@hasinstance" -> IHasInstance,
    "@scope" -> IScope,
    "@bound_args" -> IBoundArgs,
    "@bound_this" -> IBoundThis,
    "@target_function" -> ITargetFunction
  )

  def convToV(v: CValue): Value = {
    v match {
      case CLoc(loc) if notpred.contains(loc) => Loc(loc.substring(1))
      case CLoc(loc) => PredAllocSite(loc.substring(1))
      case CBoolean(b) => Bool(b)
      case CNumberLong(n) => Num(n)
      case CNumberDouble(d) => Num(d)
      case CNumberPInf => Num(Double.PositiveInfinity)
      case CNumberNInf => Num(Double.NegativeInfinity)
      case CNumberNaN => Num(Double.NaN)
      case CString(s) => Str(s)
      case CStringUnknown => StringT
      case CUndefined => Undef
      case CNull => Null
      case CIntRange(_, u) => NumberT
    }
  }
  private def Abool(ob: Option[Boolean]): Bool = {
    ob match {
      case Some(b) => b
      case None =>
        //        System.err.println("TODO for ABool")
        false
    }
  }

  val notpred = HashSet[String]("#Global")
  def loadHostEnv(cfg: CFG, model: DOMModel): JSModel = {
    // initial predefined FID
    var fid: FunctionId = -1
    def getNewFID(): FunctionId = {
      val i = fid
      fid -= 1
      i
    }
    var apis = List.empty[(String, CFGFunction)]
    val empty = Heap.empty
    val host = model.loadModel
    val semantics = model.genSemantics().asInstanceOf[String => (AbsValue, AbsState) => (AbsState, AbsState, AbsValue)]

    val heap =
      (empty /: host.list)((h, obj) => {
        obj match {
          case HObject(CLoc(loc), properties: List[HProp]) =>
            // The host objects are obviously singleton so that the location type is a system.
            val l =
              if (notpred.contains(loc))
                Loc(loc.substring(1))
              else
                PredAllocSite(loc.substring(1))

            val obj_init = Obj.empty.updatei(IPrototype, Null)

            val obj = (obj_init /: properties)((o, prop) => {
              prop match {
                case HIProp(cp, HIPropValue(value)) if cp == "@function" || cp == "@construct" =>
                  val body: CFGFunction =
                    value match {
                      case CString(file) =>
                        //parseFunctionFromFile(file, t.model.path + "/" + file, cfg)
                        JSParser.fileToAST(List(model.path + file)) match {
                          case Succ((pgm, log)) =>
                            val safeConfig = SafeConfig(CmdCFGBuild, silent = true)

                            // rewrite AST
                            val astRewriteConfig = ASTRewriteConfig()
                            val rPgm = ASTRewrite(pgm, safeConfig, astRewriteConfig).get

                            // compile
                            val compileConfig = CompileConfig()
                            val ir = Compile(rPgm, safeConfig, compileConfig).get

                            // cfg build
                            val cfgBuildConfig = CFGBuildConfig()
                            val funCFG = CFGBuild(ir, safeConfig, cfgBuildConfig).get
                            funCFG.getFunc(1).get
                          case _ => throw new InternalError("TODO")
                        }

                      case CLoc(api) => ModelCode(code = semantics(api)).getCFGFunc(cfg, api)
                      case _ => throw new InternalError("Impossible case.")
                    }

                  val fid = getNewFID()
                  body.id = fid
                  apis ::= (cp, body)

                  IValueMap.get(cp) match {
                    case Some(iv) =>
                      o.updatei(iv, FId(fid)).
                        updatei(IScope, Null)
                    case None =>
                      System.err.println(s"* An internal property '$cp' is ignored.")
                      o
                  }

                case HIProp("@proto", HIPropValue(CLoc(value))) =>
                  val v = PredAllocSite(value.substring(1))
                  o.updatei(IPrototype, v)

                case HIProp(name, HIPropValue(value)) =>
                  IValueMap.get(name) match {
                    case Some(p) =>
                      val v = convToV(value)
                      o.updatei(p, v)
                    case None =>
                      System.err.println(s"* An internal property '$name' is ignored.")
                      o
                  }

                case HUProp(name, HUPropValue(writable, enumerable, configurable, None, _, _, Some(initValue))) =>
                  //                  if (BoolTrue <= o.domIn(an)) {
                  //                    System.out.println(s"* Info: a value for getter is already defined: $name")
                  //                    o
                  //                  } else {
                  val w = Abool(writable)
                  val e = Abool(enumerable)
                  val c = Abool(configurable)
                  val v = convToV(initValue)
                  val ov = DataProp(v, w, e, c)
                  o.update(name, ov)
                //                  }

                case HUProp(name, HUPropValue(writable, enumerable, configurable, None, setter, getter, _)) =>
                  //                  val l_getters =
                  //                    getter match {
                  //                      case Some(CLoc(s)) => LocSet.empty + Loc.alpha(s, system = true)
                  //                      case None => LocSet.empty
                  //                    }
                  //                  val l_setters =
                  //                    setter match {
                  //                      case Some(CLoc(s)) => LocSet.empty + Loc.alpha(s, system = true)
                  //                      case None => LocSet.empty
                  //                    }
                  //                  val w = AbsBool.alpha(writable)
                  //                  val e = AbsBool.alpha(enumerable)
                  //                  val c = AbsBool.alpha(configurable)
                  //
                  //                  val an = AbsString.constant(name)
                  //                  if (BoolTrue <= o.domIn(an)) {
                  //                    System.out.println(s"* Info: a value for getter is already defined: $name")
                  //                    o
                  //                  } else o.update(an, PropValue.unknownValue(w, e, c, l_getters, l_setters))
                  val w = Abool(writable)
                  val e = Abool(enumerable)
                  val c = Abool(configurable)
                  val v = StringT // TODO
                  val ov = DataProp(v, w, e, c)
                  o.update(name, ov)

                case HUProp(name, HUPropValue(writable, enumerable, configurable, Some(value), _, _, _)) =>
                  val w = Abool(writable)
                  val e = Abool(enumerable)
                  val c = Abool(configurable)
                  val v = convToV(value)
                  val ov = DataProp(v, w, e, c)
                  o.update(name, ov)
                case _ => throw new InternalError("missing case")
              }
            })

            h.update(l, obj)
        }
      })
    JSModel(heap, apis, fid)
  }

  def addSnapshot(st: AbsState, snapshot: String): AbsState = {
    val concreteHeap = Heap.parse(snapshot)
    val abstractHeap = AbsHeap.alpha(concreteHeap)
    st.copy(heap = st.heap ⊔ abstractHeap)
  }
}

