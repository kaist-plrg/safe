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

// ECMASCript 5.1
object ECMAScript5 extends Model {
  lazy private val ExecutionContext = StaAddr("ExecutionContext")
  lazy private val GlobalEnvironment = StaAddr("GlobalEnvironment")
  lazy private val GlobalObject = StaAddr("GlobalObject")

  // environment
  lazy val globals: Map[Id, Value] = Map(
    // 10.3 Execution Contexts
    "ExecutionContext" -> ExecutionContext,
    // 10.2.3 The Global Environment
    "GlobalEnvironment" -> GlobalEnvironment,
    // 15.1 The Global Object
    "GlobalObject" -> GlobalObject,
    // 10.3.1 Identifier Resolution
    "IdentifierResolution" -> Clo("""(Identifier, strict) => {
      env = (ExecutionContext["LexicalEnvironment"]);
      result = GetIdentifierReference(env, Identifier, strict);
      return result;
    }"""),
    // 10.2.2.1 GetIdentifierReference (lex, name, strict)
    "GetIdentifierReference" -> Clo("""(lex, name, strict) => {
      if (lex == null) {
        reference = Reference {
          BaseValue: undefined,
          ReferencedName: name,
          StrictMode: strict
        };
        return reference;
      }
      envRec = (lex["EnvironmentRecord"]);
      exists = HasBinding(envRec, name);
      if (exists == true) {
        reference = Reference {
          BaseValue: envRec,
          ReferencedName: name,
          StrictMode: strict
        };
        return reference;
      } else {
        outer = (lex["Outer"]);
        result = GetIdentifierReference(outer, name, strict);
        return result;
      }
    }"""),
    // 8.7.1 GetValue (V)
    "GetValue" -> Clo("""(V) => {
      if (! ((typeof V) == "Reference")) {
        return V;
      }
      base = GetBase(V);
      x = IsUnresolvableReference(V);
      if x {
        throw "ReferenceError";
      }
      x = IsPropertyReference(V);
      if x {
        x = HasPrimitiveBase(V);
        if (! x) {
          get = (base ["[[Get]]"]);
        } else function get(base, P) {
          O = ToObject(base);
          desc = (O ["[[GetProperty]]"])(P);
          if (desc == undefined) return undefined;
          x = IsDataDescriptor(desc);
          if x return (desc ["[[Value]]"]);
          else {
            x = IsAccessorDescriptor(desc);
            assert x;
            getter = (desc ["[[Get]]"]);
            if (getter == undefined) return undefined;
            x = (getter ["[[Call]]"])(getter);
            return x;
          }
        }
        x1 = GetReferencedName(V);
        x2 = IsStrictReference(V);
        x = get(base, x1, x2);
        return x;
      } else {
        x1 = GetReferencedName(V);
        x2 = IsStrictReference(V);
        x = GetBindingValue(x1, x2);
        return x;
      }
    }""")
  )

  // heap
  val heap: Heap = Heap(
    ExecutionContext -> Obj("ExecutionContext", Map(
      "VariableEnvironment" -> GlobalEnvironment,
      "LexicalEnvironment" -> GlobalEnvironment,
      "ThisBinding" -> GlobalObject
    )),
    GlobalEnvironment -> Obj("LexicalEnvironment"),
    GlobalObject -> Obj("Object")
  )

  // XXX test262 modeling
  // val failClo = Clo("(x) => throw x;")
  // val assertClo = Clo("(x) => assert x;")
  // val globalClo = Clo("() => return @Global;")
  // UserId("$ERROR") -> failClo,
  // UserId("$FAIL") -> failClo,
  // UserId("runTestCase") -> assertClo,
  // UserId("fnGlobalObject") -> globalClo
}
