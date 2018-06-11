function apply (thisArg, argArray) {
    // 1. If IsCallable(func) is false, then throw a TypeError exception.
    if (@IsCallable(this) === false) throw new TypeError();
    // 2. If argArray is null or undefined, then
      // a. Return the result of calling the [[Call]] internal method of func, providing thisArg as the this value and an empty list of arguments.
    if (argArray === null || argArray === undefined) return @Call(this, thisArg, []);
    // 3. If Type(argArray) is not Object, then throw a TypeError exception.
    if (typeof argArray !== 'function' && typeof argArray !== 'object' || argArray === null) throw new TypeError();
    // 4. Let len be the result of calling the [[Get]] internal method of argArray with argument "length".
    var len = argArray.length;
    // 5. Let n be ToUint32(len).
    var n = @ToUint32(len);
    // 6. Let argList be an empty List.
    var argList = [];
    // 7. Let index be 0.
    // 8. Repeat while index < n
    for (var index = 0; index < n; index++) {
      // a. Let indexName be ToString(index).
      // b. Let nextArg be the result of calling the [[Get]] internal method of argArray with indexName as the argument.
      // c. Append nextArg as the last element of argList.
      // d. Set index to index + 1.
      argList[index] = argArray[index];
    }
    // 9. Return the result of calling the [[Call]] internal method of func, providing thisArg as the this value and argList as the list of arguments.
    return @Call(this, thisArg, argList);
  }
  