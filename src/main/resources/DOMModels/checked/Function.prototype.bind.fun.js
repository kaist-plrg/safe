function bind (thisArg) {
    // 2. If IsCallable(Target) is false, throw a TypeError exception.
    if (@IsCallable(this) === false) throw new TypeError();
    // 3. Let A be a new (possibly empty) internal list of all of the argument values provided after thisArg (arg1, arg2 etc), in order.
    var A = [];
    for (var i = 1; i < arguments.length; i++) {
      A[i-1] = arguments[i];
    }
    // 4. Let F be a new native ECMAScript object.
    var F = {};
    // 5. Set all the internal methods, except for [[Get]], of F as specified in 8.12.
    // 6. Set the [[Get]] internal property of F as specified in 15.3.5.4.
    // 15.3.5.4. NOTE: Function objects created using Function.prototype.bind use the default [[Get]] internal method.
    // 7. Set the [[TargetFunction]] internal property of F to Target.
    @TargetFunction(F, this);
    // 8. Set the [[BoundThis]] internal property of F to the value of thisArg.
    @BoundThis(F, thisArg);
    // 9. Set the [[BoundArgs]] internal property of F to A.
    @BoundArgs(F, A);
    // 10. Set the [[Class]] internal property of F to "Function".
    @Class(F, "Function");
    // 11. Set the [[Prototype]] internal property of F to the standard built-in Function prototype object as specified in 15.3.3.1.
    @Prototype(F, Function.prototype);
    // 12. Set the [[Call]] internal property of F as described in 15.3.4.5.1.
    @Call(F, @getLoc("BoundFunctionHelper"));
    // 13. Set the [[Construct]] internal property of F as described in 15.3.4.5.2.
    @Construct(F, @getLoc("BoundFunctionHelper"));
    // 14. Set the [[HasInstance]] internal property of F as described in 15.3.4.5.3.
    // TODO
    // 15. If the [[Class]] internal property of Target is "Function", then
    //      a. Let L be the length property of Target minus the length of A.
    //      b. Set the length own property of F to either 0 or L, whichever is larger.
    var len = 0;
    if (@Class(this) === "Function") {
      var L = this.length - A.length;
      if (L > 0) len = L;
    }
    // 16. Else set the length own property of F to 0.
    // 17. Set the attributes of the length own property of F to the values specified in 15.3.5.1.
    @DefineOwnProperty(F, "length", {
      value: len,
      writable: false,
      enumerable: false,
      configurable: false
    });
    // 18. Set the [[Extensible]] internal property of F to true.
    @Extensible(F, true);
    // 20. Call the [[DefineOwnProperty]] internal method of F with arguments "caller", PropertyDescriptor {[[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: false}, and false.
    // TODO
    // 21. Call the [[DefineOwnProperty]] internal method of F with arguments "arguments", PropertyDescriptor {[[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: false}, and false.
    // TODO
    // 22. Return F.
    return F;
  }
  