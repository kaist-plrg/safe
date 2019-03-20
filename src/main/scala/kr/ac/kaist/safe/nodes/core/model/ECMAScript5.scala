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
  // environment
  val globals: Map[Id, Value] = Map()
  // 8.6.1 Property Attributes
  //   [[Value]]
  //   [[Writable]]
  //   [[Enumerable]]
  //   [[Configurable]]
  //   [[Get]]
  //   [[Set]]
  // 8.6.2 Object Internal Properties and Methods
  //   [[Prototype]]
  //   [[Class]]
  //   [[Extensible]]
  //   [[Get]]
  //   [[GetOwnProperty]]
  //   [[GetProperty]]
  //   [[Put]]
  //   [[CanPut]]
  //   [[HasProperty]]
  //   [[Delete]]
  //   [[DefaultValue]]
  //   [[DefineOwnProperty]]
  // 8.7.1 GetValue (V)
  // 8.7.2 PutValue (V, W)
  // 8.10.1 IsAccessorDescriptor ( Desc )
  // 8.10.1 IsAccessorDescriptor ( Desc )
  // 8.10.2 IsDataDescriptor ( Desc )
  // 8.10.3 IsGenericDescriptor ( Desc )
  // 8.10.4 FromPropertyDescriptor ( Desc )
  // 8.10.5 ToPropertyDescriptor ( Obj )
  // 8.12.1 [[GetOwnProperty]] (P)
  // 8.12.2 [[GetProperty]] (P)
  // 8.12.3 [[Get]] (P)
  // 8.12.4 [[CanPut]] (P)
  // 8.12.5 [[Put]] ( P, V, Throw )
  // 8.12.6 [[HasProperty]] (P)
  // 8.12.7 [[Delete]] (P, Throw)
  // 8.12.8 [[DefaultValue]] (hint)
  // 8.12.9 [[DefineOwnProperty]] (P, Desc, Throw)
  // 9.1 ToPrimitive
  // 9.2 ToBoolean
  // 9.3 ToNumber
  // 9.4 ToInteger
  // 9.5 ToInt32: (Signed 32 Bit Integer)
  // 9.6 ToUint32: (Unsigned 32 Bit Integer)
  // 9.7 ToUint16: (Unsigned 16 Bit Integer)
  // 9.8 ToString
  // 9.8.1 ToString Applied to the Number Type
  // 9.9 ToObject
  // 9.10 CheckObjectCoercible
  // 9.11 IsCallable
  // 9.12 The SameValue Algorithm
  // 10.2.1 Environment Records
  //   HasBinding(N)
  //   CreateMutableBinding(N, D)
  //   SetMutableBinding(N,V, S)
  //   GetBindingValue(N,S)
  //   DeleteBinding(N)
  //   ImplicitThisValue()
  // 10.2.1.1.1 HasBinding(N)
  // 10.2.1.1.2 CreateMutableBinding (N, D)
  // 10.2.1.1.3 SetMutableBinding (N,V,S)
  // 10.2.1.1.4 GetBindingValue(N,S)
  // 10.2.1.1.5 DeleteBinding (N)
  // 10.2.1.1.6 ImplicitThisValue()
  // 10.2.1.1.7 CreateImmutableBinding (N)
  // 10.2.1.1.8 InitializeImmutableBinding (N,V)
  // 10.2.1.2.1 HasBinding(N)
  // 10.2.1.2.2 CreateMutableBinding (N, D)
  // 10.2.1.2.3 SetMutableBinding (N,V,S)
  // 10.2.1.2.4 GetBindingValue(N,S)
  // 10.2.1.2.5 DeleteBinding (N)
  // 10.2.1.2.6 ImplicitThisValue()
  // 10.2.2.1 GetIdentifierReference (lex, name, strict)
  // 10.2.2.2 NewDeclarativeEnvironment (E)
  // 10.2.2.3 NewObjectEnvironment (O, E)

  // 10.3.1 Identifier Resolution
  // 10.4.1 Entering Global Code
  // 10.4.1.1 Initial Global Execution Context
  // 10.4.2 Entering Eval Code
  // 10.4.3 Entering Function Code
  // 10.5 Declaration Binding Instantiation
  // 10.6 Arguments Object
  //   MakeArgGetter
  //   MakeArgSetter
  //   [[Get]]
  //   [[GetOwnProperty]]
  //   [[DefineOwnProperty]]
  //   [[Delete]]
  // 11.1.4 Array Initialiser
  //   ArrayLiteral : [ Elisionopt ]
  //   ArrayLiteral : [ ElementList ]
  //   ArrayLiteral : [ ElementList , Elisionopt ]
  //   ElementList : Elisionopt AssignmentExpression
  //   ElementList : ElementList , Elisionopt AssignmentExpression
  //   Elision : ,
  //   Elision : Elision ,
  // 11.1.5 Object Initialiser
  //   ObjectLiteral : { }
  //   ObjectLiteral : { PropertyNameAndValueList }
  //   ObjectLiteral : { PropertyNameAndValueList ,}
  //   PropertyNameAndValueList : PropertyAssignment
  //   PropertyNameAndValueList : PropertyNameAndValueList , PropertyAssignment
  //   PropertyAssignment:PropertyName:AssignmentExpression
  //   PropertyAssignment : get PropertyName ( ) { FunctionBody }
  //   PropertyAssignment : set PropertyName ( PropertySetParameterList ) { FunctionBody }
  //   PropertyName : IdentifierName
  //   PropertyName : StringLiteral
  //   PropertyName : NumericLiteral
  // 11.2.1 Property Accessors
  //   MemberExpression : MemberExpression [ Expression ]
  //   CallExpression : CallExpression [ Expression ]
  // 11.2.2 ThenewOperator
  //   NewExpression : new NewExpression
  //   MemberExpression : new MemberExpression Arguments
  // 11.2.3 Function Calls
  //   CallExpression : MemberExpression Arguments
  // 11.2.4 Argument Lists
  //   Arguments : ( )
  //   Arguments : ( ArgumentList )
  //   ArgumentList: AssignmentExpression
  //   ArgumentList : ArgumentList , AssignmentExpression
  // 11.2.5 Function Expressions
  //    MemberExpression : FunctionExpression
  // 11.3.1 Postfix Increment Operator
  //    PostfixExpression : LeftHandSideExpression [no LineTerminator here] ++
  // 11.3.2 Postfix Decrement Operator
  //    PostfixExpression : LeftHandSideExpression [no LineTerminator here] --
  // 11.4.1 The delete Operator
  //   UnaryExpression : delete UnaryExpression
  // 11.4.2 The void Operator
  //   UnaryExpression : void UnaryExpression
  // 11.4.3 The typeof Operator
  //   UnaryExpression : typeof UnaryExpression
  // 11.4.4 Prefix Increment Operator
  //   UnaryExpression : ++ UnaryExpression
  // 11.4.5 Prefix Decrement Operator
  //   UnaryExpression : -- UnaryExpression
  // 11.4.6 Unary + Operator
  //   UnaryExpression : + UnaryExpression
  // 11.4.7 Unary-Operator
  //   UnaryExpression : - UnaryExpression
  // 11.4.8 Bitwise NOT Operator ( ~ )
  //   UnaryExpression : ~ UnaryExpression
  // 11.4.9 Logical NOT Operator ( ! )
  //   UnaryExpression : ! UnaryExpression
  // 11.5 Multiplicative Operators
  //   MultiplicativeExpression : MultiplicativeExpression @ UnaryExpression
  // 11.6.1 The Addition operator ( + )
  //   AdditiveExpression : AdditiveExpression + MultiplicativeExpression
  // 11.6.2 The Subtraction Operator ( - )
  //   AdditiveExpression : AdditiveExpression - MultiplicativeExpression
  // 11.7.1 The Left Shift Operator ( << )
  //   ShiftExpression : ShiftExpression << AdditiveExpression
  // 11.7.2 The Signed Right Shift Operator ( >> )
  //   ShiftExpression : ShiftExpression >> AdditiveExpression
  // 11.7.3 The Unsigned Right Shift Operator ( >>> )
  //   ShiftExpression : ShiftExpression >>> AdditiveExpression
  // 11.8.1 The Less-than Operator ( < )
  //   RelationalExpression : RelationalExpression < ShiftExpression
  // 11.8.2 The Greater-than Operator ( > )
  //   RelationalExpression : RelationalExpression > ShiftExpression
  //   11.8.3 The Less-than-or-equal Operator ( <= )
  //   RelationalExpression : RelationalExpression <= ShiftExpression
  // 11.8.4 The Greater-than-or-equal Operator ( >= )
  //   RelationalExpression : RelationalExpression >= ShiftExpression
  // 11.8.5 The Abstract Relational Comparison Algorithm
  // 11.8.6 The instanceof operator
  //   RelationalExpression: RelationalExpression instanceof ShiftExpression
  // 11.8.7 The in operator
  //   RelationalExpression : RelationalExpression in ShiftExpression
  // 11.9.1 The Equals Operator ( == )
  //   EqualityExpression : EqualityExpression == RelationalExpression
  // 11.9.2 The Does-not-equals Operator ( != )
  //   EqualityExpression : EqualityExpression != RelationalExpression
  // 11.9.3 The Abstract Equality Comparison Algorithm
  // 11.9.4 The Strict Equals Operator ( === )
  //   EqualityExpression : EqualityExpression === RelationalExpression
  // 11.9.5 The Strict Does-not-equal Operator ( !== )
  //   EqualityExpression : EqualityExpression !== RelationalExpression
  // 11.9.6 The Strict Equality Comparison Algorithm
  // 11.10 Binary Bitwise Operators
  //   A : A @ B
  // 11.11 Binary Logical Operators
  //   LogicalANDExpression : LogicalANDExpression && BitwiseORExpression
  //   LogicalORExpression : LogicalORExpression || LogicalANDExpression
  // 11.12 Conditional Operator ( ? : )
  //   ConditionalExpression : LogicalORExpression ? AssignmentExpression : AssignmentExpression
  // 11.13 Assignment Operators
  // 11.13.1 Simple Assignment ( = )
  //   AssignmentExpression : LeftHandSideExpression = AssignmentExpression
  // 11.13.2 Compound Assignment ( op= )
  //   AssignmentExpression : LeftHandSideExpression @ = AssignmentExpression
  // 11.14 Comma Operator ( , )
  //   Expression : Expression , AssignmentExpression
  // 12.1 Block
  //   Block : { }
  //   Block : { StatementList }
  //   StatementList : Statement
  //   StatementList : StatementList Statement
  // 12.2 Variable Statement
  //   VariableStatement : var VariableDeclarationList ;
  //   VariableDeclarationList :VariableDeclaration
  //   VariableDeclarationList : VariableDeclarationList , VariableDeclaration
  //   VariableDeclaration : Identifier
  //   VariableDeclaration : Identifier Initialiser
  //   Initialiser : = AssignmentExpression
  // 12.2.1 Strict Mode Restrictions
  // 12.3 Empty Statement
  //   EmptyStatement : ;
  // 12.4 Expression Statement
  //   ExpressionStatement : [lookahead  {{, function}] Expression;
  // 12.5 The if Statement
  //   IfStatement : if ( Expression ) Statement else Statement
  //   IfStatement : if ( Expression ) Statement
  // 12.6.1 The do-while Statement
  //   do Statement while ( Expression );
  // 12.6.2 The while Statement
  //   IterationStatement : while ( Expression ) Statement
  // 12.6.3 The for Statement
  //   IterationStatement : for (ExpressionNoInopt ; Expressionopt ; Expressionopt) Statement
  //   IterationStatement : for ( var VariableDeclarationListNoIn ; Expressionopt ; Expressionopt ) Statement
  // 12.6.4 The for-in Statement
  //   IterationStatement : for ( LeftHandSideExpression in Expression ) Statement
  //   IterationStatement : for ( var VariableDeclarationNoIn in Expression ) Statement
  // 12.7 The continue Statement
  // 12.8 The break Statement
  // 12.9 The return Statement
  // 12.10 The with Statement
  //   WithStatement : with ( Expression ) Statement
  // 12.11 The switch Statement
  //   SwitchStatement : switch ( Expression ) CaseBlock
  //   CaseBlock : { CaseClausesopt }
  //   CaseBlock : { CaseClausesopt DefaultClause CaseClausesopt }
  //   CaseClause : case Expression : StatementListopt
  // 12.12 Labelled Statements
  //   Identifier : Statement
  // 12.13 The throw Statement
  //   ThrowStatement : throw [no LineTerminator here] Expression ;
  // 12.14 The try Statement
  //   TryStatement : try Block Catch
  //   TryStatement : try Block Finally
  //   TryStatement : try Block Catch Finally
  //   Catch : catch ( Identifier ) Block
  //   Finally : finally Block
  // 12.15 The debugger statement
  //   DebuggerStatement : debugger ;
  //   FunctionDeclaration : function Identifier ( FormalParameterListopt ) { FunctionBody }
  //   FunctionExpression : function ( FormalParameterListopt ) { FunctionBody }
  //   FunctionExpression : function Identifier ( FormalParameterListopt ) { FunctionBody }
  //   FunctionBody : SourceElementsopt
  // 13.2 Creating Function Objects
  // 13.2.1 [[Call]]
  // 13.2.2 [[Construct]]
  // 14 Program
  //   Program : SourceElementsopt
  //   SourceElements : SourceElements SourceElement
  //   SourceElement : Statement
  //   SourceElement : FunctionDeclaration
  // 15.1.1 Value Properties of the Global Object
  // 15.1.1.1 NaN
  // 15.1.1.2 Infinity
  // 15.1.1.3 undefined
  // 15.1.2 Function Properties of the Global Object
  // 15.1.2.1 eval (x)
  // 15.1.2.1.1 Direct Call to Eval
  // 15.1.2.2 parseInt (string , radix)
  // 15.1.2.3 parseFloat (string)
  // 15.1.2.4 isNaN (number)
  // 15.1.2.5 isFinite (number)
  // 15.1.3 URI Handling Function Properties
  // 15.1.3.1 decodeURI (encodedURI)
  // 15.1.3.2 decodeURIComponent (encodedURIComponent)
  // 15.1.3.3 encodeURI (uri)
  // 15.1.3.4 encodeURIComponent (uriComponent)
  // 15.1.4 Constructor Properties of the Global Object
  // 15.1.5.2 JSON
  // 15.2.1 The Object Constructor Called as a Function
  // 15.2.1.1 Object ( [ value ] )
  // 15.2.2 The Object Constructor
  // 15.2.2.1 new Object ( [ value ] )
  // 15.2.3 Properties of the Object Constructor
  // 15.2.3.1 Object.prototype
  // 15.2.3.2 Object.getPrototypeOf ( O )
  // 15.2.3.3 Object.getOwnPropertyDescriptor ( O, P )
  // 15.2.3.4 Object.getOwnPropertyNames ( O )
  // 15.2.3.5 Object.create ( O [, Properties] )
  // 15.2.3.6 Object.defineProperty ( O, P, Attributes )
  // 15.2.3.7 Object.defineProperties ( O, Properties )
  // 15.2.3.8 Object.seal ( O )
  // 15.2.3.9 Object.freeze ( O )
  // 15.2.3.10 Object.preventExtensions ( O )
  // 15.2.3.11 Object.isSealed ( O )
  // 15.2.3.12 Object.isFrozen ( O )
  // 15.2.3.13 Object.isExtensible ( O )
  // 15.2.3.14 Object.keys ( O )
  // 15.2.4 Properties of the Object Prototype Object
  // 15.2.4.1 Object.prototype.constructor
  // 15.2.4.2 Object.prototype.toString ( )
  // 15.2.4.3 Object.prototype.toLocaleString ( )
  // 15.2.4.4 Object.prototype.valueOf ( )
  // 15.2.4.5 Object.prototype.hasOwnProperty (V)
  // 15.2.4.6 Object.prototype.isPrototypeOf (V)
  // 15.2.4.7 Object.prototype.propertyIsEnumerable (V)
  // 15.2.5 Properties of Object Instances
  // 15.3.1 The Function Constructor Called as a Function
  // 15.3.1.1 Function (p1, p2, ... , pn, body)
  // 15.3.2 The Function Constructor
  // 15.3.2.1 new Function (p1, p2, ... , pn, body)
  // 15.3.3 Properties of the Function Constructor
  // 15.3.3.1 Function.prototype
  // 15.3.3.2 Function.length
  // 15.3.4 Properties of the Function Prototype Object
  // 15.3.4.1 Function.prototype.constructor
  // 15.3.4.2 Function.prototype.toString ( )
  // 15.3.4.3 Function.prototype.apply (thisArg, argArray)
  // 15.3.4.4 Function.prototype.call (thisArg [ , arg1 [ , arg2, ... ] ] )
  // 15.3.4.5 Function.prototype.bind (thisArg [, arg1 [, arg2, ...]])
  // 15.3.4.5.1 [[Call]]
  // 15.3.4.5.2 [[Construct]]
  // 15.3.4.5.3 [[HasInstance]] (V)
  // 15.3.5 Properties of Function Instances
  // 15.3.5.1 length
  // 15.3.5.2 prototype
  // 15.3.5.3 [[HasInstance]] (V)
  // 15.3.5.4 [[Get]] (P)
  // 15.4.1 The Array Constructor Called as a Function
  // 15.4.1.1 Array ( [ item1 [ , item2 [ , ... ] ] ] )
  // 15.4.2 The Array Constructor
  // 15.4.2.1 new Array ( [ item0 [ , item1 [ , ... ] ] ] )
  // 15.4.2.2 new Array (len)
  // 15.4.3 Properties of the Array Constructor
  // 15.4.3.1 Array.prototype
  // 15.4.3.2 Array.isArray ( arg )
  // 15.4.4 Properties of the Array Prototype Object
  // 15.4.4.1 Array.prototype.constructor
  // 15.4.4.2 Array.prototype.toString ( )
  // 15.4.4.3 Array.prototype.toLocaleString ( )
  // 15.4.4.4 Array.prototype.concat ( [ item1 [ , item2 [ , ... ] ] ] )
  // 15.4.4.5 Array.prototype.join (separator)
  // 15.4.4.6 Array.prototype.pop ( )
  // 15.4.4.7 Array.prototype.push ( [ item1 [ , item2 [ , ... ] ] ] )
  // 15.4.4.8 Array.prototype.reverse ( )
  // 15.4.4.9 Array.prototype.shift ( )
  // 15.4.4.10 Array.prototype.slice (start, end)
  // 15.4.4.11 Array.prototype.sort (comparefn)
  // 15.4.4.12 Array.prototype.splice (start, deleteCount [ , item1 [ , item2 [ , ... ] ] ] )
  // 15.4.4.13 Array.prototype.unshift ( [ item1 [ , item2 [ , ... ] ] ] )
  // 15.4.4.14 Array.prototype.indexOf ( searchElement [ , fromIndex ] )
  // 15.4.4.15 Array.prototype.lastIndexOf ( searchElement [ , fromIndex ] )
  // 15.4.4.16 Array.prototype.every ( callbackfn [ , thisArg ] )
  // 15.4.4.17 Array.prototype.some ( callbackfn [ , thisArg ] )
  // 15.4.4.18 Array.prototype.forEach ( callbackfn [ , thisArg ] )
  // 15.4.4.19 Array.prototype.map ( callbackfn [ , thisArg ] )
  // 15.4.4.20 Array.prototype.filter ( callbackfn [ , thisArg ] )
  // 15.4.4.21 Array.prototype.reduce ( callbackfn [ , initialValue ] )
  // 15.4.4.22 Array.prototype.reduceRight ( callbackfn [ , initialValue ] )
  // 15.4.5 Properties of Array Instances
  // 15.4.5.1 [[DefineOwnProperty]] ( P, Desc, Throw )
  // 15.4.5.2 length
  // 15.5.1 The String Constructor Called as a Function
  // 15.5.1.1 String ( [ value ] )
  // 15.5.2 The String Constructor
  // 15.5.2.1 new String ( [ value ] )
  // 15.5.3 Properties of the String Constructor
  // 15.5.3.1 String.prototype
  // 15.5.3.2 String.fromCharCode ( [ char0 [ , char1 [ , ... ] ] ] )
  // 15.5.4 Properties of the String Prototype Object
  // 15.5.4.1 String.prototype.constructor
  // 15.5.4.2 String.prototype.toString ( )
  // 15.5.4.3 String.prototype.valueOf ( )
  // 15.5.4.4 String.prototype.charAt (pos)
  // 15.5.4.5 String.prototype.charCodeAt (pos)
  // 15.5.4.6 String.prototype.concat ( [ string1 [ , string2 [ , ... ] ] ] )
  // 15.5.4.7 String.prototype.indexOf (searchString, position)
  // 15.5.4.8 String.prototype.lastIndexOf (searchString, position)
  // 15.5.4.9 String.prototype.localeCompare (that)
  // 15.5.4.10 String.prototype.match (regexp)
  // 15.5.4.11 String.prototype.replace (searchValue, replaceValue)
  // 15.5.4.12 String.prototype.search (regexp)
  // 15.5.4.13 String.prototype.slice (start, end)
  // 15.5.4.14 String.prototype.split (separator, limit)
  // 15.5.4.15 String.prototype.substring (start, end)
  // 15.5.4.16 String.prototype.toLowerCase ( )
  // 15.5.4.17 String.prototype.toLocaleLowerCase ( )
  // 15.5.4.18 String.prototype.toUpperCase ( )
  // 15.5.4.19 String.prototype.toLocaleUpperCase ( )
  // 15.5.4.20 String.prototype.trim ( )
  // 15.5.5 Properties of String Instances
  // 15.5.5.1 length
  // 15.5.5.2 [[GetOwnProperty]] ( P )
  // 15.6.1 The Boolean Constructor Called as a Function
  // 15.6.1.1 Boolean (value)
  // 15.6.2 The Boolean Constructor
  // 15.6.2.1 new Boolean (value)
  // 15.6.3 Properties of the Boolean Constructor
  // 15.6.3.1 Boolean.prototype
  // 15.6.4 Properties of the Boolean Prototype Object
  // 15.6.4.1 Boolean.prototype.constructor
  // 15.6.4.2 Boolean.prototype.toString ( )
  // 15.6.4.3 Boolean.prototype.valueOf ( )
  // 15.6.5 Properties of Boolean Instances
  // 15.7.1 The Number Constructor Called as a Function
  // 15.7.1.1 Number ( [ value ] )
  // 15.7.2 The Number Constructor
  // 15.7.2.1 new Number ( [ value ] )
  // 15.7.3 Properties of the Number Constructor
  // 15.7.3.1 Number.prototype
  // 15.7.3.2 Number.MAX_VALUE
  // 15.7.3.3 Number.MIN_VALUE
  // 15.7.3.4 Number.NaN
  // 15.7.3.5 Number.NEGATIVE_INFINITY
  // 15.7.3.6 Number.POSITIVE_INFINITY
  // 15.7.4 Properties of the Number Prototype Object
  // 15.7.4.1 Number.prototype.constructor
  // 15.7.4.2 Number.prototype.toString ( [ radix ] )
  // 15.7.4.3 Number.prototype.toLocaleString()
  // 15.7.4.4 Number.prototype.valueOf ( )
  // 15.7.4.5 Number.prototype.toFixed (fractionDigits)
  // 15.7.4.6 Number.prototype.toExponential (fractionDigits)
  // 15.7.4.7 Number.prototype.toPrecision (precision)
  // 15.7.5 Properties of Number Instances
  // 15.8 The Math Object
  // 15.8.1 Value Properties of the Math Object
  // 15.8.1.1 E
  // 15.8.1.2 LN10
  // 15.8.1.3 LN2
  // 15.8.1.4 LOG2E
  // 15.8.1.5 LOG10E
  // 15.8.1.6 PI
  // 15.8.1.7 SQRT1_2
  // 15.8.1.8 SQRT2
  // 15.8.2 Function Properties of the Math Object
  // 15.8.2.1 abs (x)
  // 15.8.2.2 acos (x)
  // 15.8.2.3 asin (x)
  // 15.8.2.4 atan (x)
  // 15.8.2.5 atan2 (y, x)
  // 15.8.2.6 ceil (x)
  // 15.8.2.7 cos (x)
  // 15.8.2.8 exp (x)
  // 15.8.2.9 floor (x)
  // 15.8.2.10 log (x)
  // 15.8.2.11 max ( [ value1 [ , value2 [ , ... ] ] ] )
  // 15.8.2.12 min ( [ value1 [ , value2 [ , ... ] ] ] )
  // 15.8.2.13 pow (x, y)
  // 15.8.2.14 random ( )
  // 15.8.2.15 round (x)
  // 15.8.2.16 sin (x)
  // 15.8.2.17 sqrt (x)
  // 15.8.2.18 tan (x)
  // 15.9.1 Overview of Date Objects and Definitions of Abstract Operators
  // 15.9.1.1 Time Values and Time Range
  // 15.9.1.2 Day Number and Time within Day
  // 15.9.1.3 Year Number
  // 15.9.1.4 Month Number
  // 15.9.1.5 Date Number
  // 15.9.1.6 Week Day
  // 15.9.1.7 Local Time Zone Adjustment
  // 15.9.1.8 Daylight Saving Time Adjustment
  // 15.9.1.9 Local Time
  // 15.9.1.10 Hours, Minutes, Second, and Milliseconds
  // 15.9.1.11 MakeTime (hour, min, sec, ms)
  // 15.9.1.12 MakeDay (year, month, date)
  // 15.9.1.13 MakeDate (day, time)
  // 15.9.1.14 TimeClip (time)
  // 15.9.1.15 Date Time String Format
  // 15.9.1.15.1 Extended years
  // 15.9.2 The Date Constructor Called as a Function
  // 15.9.2.1 Date ( [ year [, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] ] ] )
  // 15.9.3 The Date Constructor
  // 15.9.3.1 new Date (year, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] )
  // 15.9.3.2 new Date (value)
  // 15.9.3.3 new Date ( )
  // 15.9.4 Properties of the Date Constructor
  // 15.9.4.1 Date.prototype
  // 15.9.4.2 Date.parse (string)
  // 15.9.4.3 Date.UTC (year, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] )
  // 15.9.4.4 Date.now ( )
  // 15.9.5 Properties of the Date Prototype Object
  // 15.9.5.1 Date.prototype.constructor
  // 15.9.5.2 Date.prototype.toString ( )
  // 15.9.5.3 Date.prototype.toDateString ( )
  // 15.9.5.4 Date.prototype.toTimeString ( )
  // 15.9.5.5 Date.prototype.toLocaleString ( )
  // 15.9.5.6 Date.prototype.toLocaleDateString ( )
  // 15.9.5.7 Date.prototype.toLocaleTimeString ( )
  // 15.9.5.8 Date.prototype.valueOf ( )
  // 15.9.5.9 Date.prototype.getTime ( )
  // 15.9.5.10 Date.prototype.getFullYear ( )
  // 15.9.5.11 Date.prototype.getUTCFullYear ( )
  // 15.9.5.12 Date.prototype.getMonth ( )
  // 15.9.5.13 Date.prototype.getUTCMonth ( )
  // 15.9.5.14 Date.prototype.getDate ( )
  // 15.9.5.15 Date.prototype.getUTCDate ( )
  // 15.9.5.16 Date.prototype.getDay ( )
  // 15.9.5.17 Date.prototype.getUTCDay ( )
  // 15.9.5.18 Date.prototype.getHours ( )
  // 15.9.5.19 Date.prototype.getUTCHours ( )
  // 15.9.5.20 Date.prototype.getMinutes ( )
  // 15.9.5.21 Date.prototype.getUTCMinutes ( )
  // 15.9.5.22 Date.prototype.getSeconds ( )
  // 15.9.5.23 Date.prototype.getUTCSeconds ( )
  // 15.9.5.24 Date.prototype.getMilliseconds ( )
  // 15.9.5.25 Date.prototype.getUTCMilliseconds ( )
  // 15.9.5.26 Date.prototype.getTimezoneOffset ( )
  // 15.9.5.27 Date.prototype.setTime (time)
  // 15.9.5.28 Date.prototype.setMilliseconds (ms)
  // 15.9.5.29 Date.prototype.setUTCMilliseconds (ms)
  // 15.9.5.30 Date.prototype.setSeconds (sec [, ms ] )
  // 15.9.5.31 Date.prototype.setUTCSeconds (sec [, ms ] )
  // 15.9.5.32 Date.prototype.setMinutes (min [, sec [, ms ] ] )
  // 15.9.5.33 Date.prototype.setUTCMinutes (min [, sec [, ms ] ] )
  // 15.9.5.34 Date.prototype.setHours (hour [, min [, sec [, ms ] ] ] )
  // 15.9.5.35 Date.prototype.setUTCHours (hour [, min [, sec [, ms ] ] ] )
  // 15.9.5.36 Date.prototype.setDate (date)
  // 15.9.5.37 Date.prototype.setUTCDate (date)
  // 15.9.5.38 Date.prototype.setMonth (month [, date ] )
  // 15.9.5.39 Date.prototype.setUTCMonth (month [, date ] )
  // 15.9.5.40 Date.prototype.setFullYear (year [, month [, date ] ] )
  // 15.9.5.41 Date.prototype.setUTCFullYear (year [, month [, date ] ] )
  // 15.9.5.42 Date.prototype.toUTCString ( )
  // 15.9.5.43 Date.prototype.toISOString ( )
  // 15.9.5.44 Date.prototype.toJSON ( key )
  // 15.9.6 Properties of Date Instances
  // 15.10.1 Patterns
  // 15.10.2 Pattern Semantics
  // 15.10.2.1 Notation
  // 15.10.2.2 Pattern
  // 15.10.2.3 Disjunction
  // 15.10.2.4 Alternative
  // 15.10.2.5 Term
  // 15.10.2.6 Assertion
  // 15.10.2.7 Quantifier
  // 15.10.2.8 Atom
  // 15.10.2.9 AtomEscape
  // 15.10.2.10 CharacterEscape
  // 15.10.2.11 DecimalEscape
  // 15.10.2.12 CharacterClassEscape
  // 15.10.2.13 CharacterClass
  // 15.10.2.14 ClassRanges
  // 15.10.2.15 NonemptyClassRanges
  // 15.10.2.16 NonemptyClassRangesNoDash
  // 15.10.2.17 ClassAtom
  // 15.10.2.18 ClassAtomNoDash
  // 15.10.2.19 ClassEscape
  // 15.10.3 The RegExp Constructor Called as a Function
  // 15.10.3.1 RegExp(pattern, flags)
  // 15.10.4 The RegExp Constructor
  // 15.10.4.1 new RegExp(pattern, flags)
  // 15.10.5 Properties of the RegExp Constructor
  // 15.10.5.1 RegExp.prototype
  // 15.10.6 Properties of the RegExp Prototype Object
  // 15.10.6.1 RegExp.prototype.constructor
  // 15.10.6.2 RegExp.prototype.exec(string)
  // 15.10.6.3 RegExp.prototype.test(string)
  // 15.10.6.4 RegExp.prototype.toString()
  // 15.10.7 Properties of RegExp Instances
  // 15.10.7.1 source
  // 15.10.7.2 global
  // 15.10.7.3 ignoreCase
  // 15.10.7.4 multiline
  // 15.10.7.5 lastIndex
  // 15.11.1 The Error Constructor Called as a Function
  // 15.11.1.1 Error (message)
  // 15.11.2 The Error Constructor
  // 15.11.2.1 new Error (message)
  // 15.11.3 Properties of the Error Constructor
  // 15.11.4 Properties of the Error Prototype Object
  // 15.11.4.1 Error.prototype.constructor
  // 15.11.4.2 Error.prototype.name
  // 15.11.4.3 Error.prototype.message
  // 15.11.4.4 Error.prototype.toString ( )
  // 15.11.5 Properties of Error Instances
  // 15.11.6 Native Error Types Used in This Standard
  // 15.11.6.1 EvalError
  // 15.11.6.2 RangeError
  // 15.11.6.3 ReferenceError
  // 15.11.6.4 SyntaxError
  // 15.11.6.5 TypeError
  // 15.11.6.6 URIError
  // 15.11.7 NativeError Object Structure
  // 15.11.7.1 NativeError Constructors Called as Functions
  // 15.11.7.2 NativeError (message)
  // 15.11.7.3 The NativeError Constructors
  // 15.11.7.4 New NativeError (message)
  // 15.11.7.5 Properties of the NativeError Constructors
  // 15.11.7.6 NativeError.prototype
  // 15.11.7.7 Properties of the NativeError Prototype Objects
  // 15.11.7.8 NativeError.prototype.constructor
  // 15.11.7.9 NativeError.prototype.name
  // 15.11.7.10 NativeError.prototype.message
  // 15.11.7.11 Properties of NativeError Instances
  // 15.12.1 The JSON Grammar
  // 15.12.1.1 The JSON Lexical Grammar
  // 15.12.1.2 The JSON Syntactic Grammar
  // 15.12.2 parse ( text [ , reviver ] )
  // 15.12.3 stringify ( value [ , replacer [ , space ] ] )

  // heap
  val heap: Heap = Heap(Map())

  // XXX test262 modeling
  // val failClo = Clo("(x) => throw x;")
  // val assertClo = Clo("(x) => assert x;")
  // val globalClo = Clo("() => return @Global;")
  // UserId("$ERROR") -> failClo,
  // UserId("$FAIL") -> failClo,
  // UserId("runTestCase") -> assertClo,
  // UserId("fnGlobalObject") -> globalClo
}
