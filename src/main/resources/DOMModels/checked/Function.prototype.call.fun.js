function call (thisArg) {
    // 1. If IsCallable(func) is false, then throw a TypeError exception.
    if (@IsCallable(this) === false) throw new TypeError();
    // 2. Let argList be an empty List.
    var argList = [];
    // 3. If this method was called with more than one argument then in left to right order starting with arg1 append each argument as the last element of argList
    for (var i = 1; i < arguments.length; i++) {
      argList[i-1] = arguments[i];
    }
    // 4. Return the result of calling the [[Call]] internal method of func, providing thisArg as the this value and argList as the list of arguments.
    return @Call(this, thisArg, argList);
  }
  