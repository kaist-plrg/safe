function unshift () {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 3. Let len be ToUint32(lenVal).
    var len = @ToUint32(lenVal);
    // 4. Let argCount be the number of actual arguments.
    var argCount = arguments.length;
    // 5. Let k be len.
    var k = len;
    // 6. Repeat, while k > 0,
    while (k > 0) {
      // a. Let from be ToString(k–1).
      var from = @ToString(k-1);
      // b. Let to be ToString(k+argCount –1).
      var to = @ToString(k + argCount - 1);
      // c. Let fromPresent be the result of calling the [[HasProperty]] internal method of O with argument from.
      var fromPresent = (from in O);
      // d. If fromPresent is true, then
      if (fromPresent) {
        // i. Let fromValue be the result of calling the [[Get]] internal method of O with argument from.
        var fromValue = O[from];
        // ii. Call the [[Put]] internal method of O with arguments to, fromValue, and true.
        O[to] = fromValue;
      }
      // e. Else, fromPresent is false
      else {
        // i. Call the [[Delete]] internal method of O with arguments to, and true.
        delete O[to];
      }
      // f. Decrease k by 1.
      k -= 1;
    }
    // 7. Let j be 0.
    var j = 0;
    // 8. Let items be an internal List whose elements are, in left to right order, the arguments that were passed to this function invocation.
    var items = arguments;
    // 9. Repeat, while items is not empty
    for (var i = 0; i < items.length; i++) {
      // a. Remove the first element from items and let E be the value of that element.
      var E = items[i];
      // b. Call the [[Put]] internal method of O with arguments ToString(j), E, and true.
      O[@ToString(j)] = E;
      // c. Increase j by 1.
      j += 1;
    }
    // 10. Call the [[Put]] internal method of O with arguments "length", len+argCount, and true.
    O.length = len + argCount;
    // 11. Return len+argCount.
    return len + argCount;
  }
  