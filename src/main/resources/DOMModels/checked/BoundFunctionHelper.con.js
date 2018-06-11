function bindConstruct () {
    // 1. Let target be the value of F’s [[TargetFunction]] internal property.
    var target = @TargetFunction(arguments.callee);
    // 2. If target has no [[Construct]] internal method, a TypeError exception is thrown.
    if (@HasConstruct(target) === false) throw new TypeError();
    // 3. Let boundArgs be the value of F’s [[BoundArgs]] internal property.
    var boundArgs = @BoundArgs(arguments.callee);
    // 4. Let args be a new list containing the same values as the list boundArgs in the same order followed by the same values as the list ExtraArgs in the same order.
    var args = [];
    for (var i = 0; i < boundArgs.length; i++) {
      args[i] = boundArgs[i];
    }
    for (var j = 0; j < arguments.length; j++) {
      args[j+i] = arguments[j];
    }
    // 5. Return the result of calling the [[Construct]] internal method of target providing args as the arguments.
    return @Construct(target, this, args);
  }
  