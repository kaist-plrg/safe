function bindCall () {
    // 1. Let boundArgs be the value of F’s [[BoundArgs]] internal property.
    var boundArgs = @BoundArgs(arguments.callee);
    // 2. Let boundThis be the value of F’s [[BoundThis]] internal property.
    var boundThis = @BoundThis(arguments.callee);
    // 3. Let target be the value of F’s [[TargetFunction]] internal property.
    var target = @TargetFunction(arguments.callee);
    // 4. Let args be a new list containing the same values as the list boundArgs in the same order followed by the same values as the list ExtraArgs in the same order.
    var args = [];
    for (var i = 0; i < boundArgs.length; i++) {
      args[i] = boundArgs[i];
    }
    for (var j = 0; j < arguments.length; j++) {
      args[j+i] = arguments[j];
    }
    // 5. Return the result of calling the [[Call]] internal method of target providing boundThis as the this value and providing args as the arguments.
    return @Call(target, boundThis, args);
  }
  