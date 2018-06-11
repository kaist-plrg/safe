function reduce(callbackfn, initialValue) {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenValue be the result of calling the [[Get]] internal method of O with the argument "length".
    var lenValue = O.length;
    // 3. Let len be ToUint32(lenValue).
    var len = @ToUint32(lenValue);
    // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
    if (@IsCallable(callbackfn) === false) {
      throw new TypeError();
    }
    // 5. If len is 0 and initialValue is not present, throw a TypeError exception.
    if (len === 0 && arguments.length < 2) {
      throw new TypeError();
    }
    // 6. Let k be 0.
    var k = 0;
    // 7. If initialValue is present, then
    if (arguments.length > 1) {
      // a. Set accumulator to initialValue.
      var accumulator = initialValue;
    }
    // 8. Else, initialValue is not present
    else {
      // a. Let kPresent be false.
      var kPresent = false;
      // b. Repeat, while kPresent is false and k < len
      while (kPresent === false && k < len) {
        // i. Let Pk be ToString(k).
        var Pk = @ToString(k);
        //ii. Let kPresent be the result of calling the [[HasProperty]] internal method of O with argument Pk.
        var kPresent = (Pk in O);
        // iii. If kPresent is true, then
        if (kPresent) {
          // 1. Let accumulator be the result of calling the [[Get]] internal method of O with argument Pk.
          var accumulator = O[Pk];
        }
        // iv. Increase k by 1.
        k += 1;
      }
      // c. If kPresent is false, throw a TypeError exception.
      if (!kPresent) {
        throw new TypeError();
      }
    }
    // 9. Repeat, while k < len
    while (k < len) {
      // a. Let Pk be ToString(k).
      var Pk = @ToString(k);
      // b. Let kPresent be the result of calling the [[HasProperty]] internal method of O with argument Pk.
      var kPresent = (Pk in O);
      // c. If kPresent is true, then
      if (kPresent) {
        // i. Let kValue be the result of calling the [[Get]] internal method of O with argument Pk.
        var kValue = O[Pk];
        // ii. Let accumulator be the result of calling the [[Call]] internal method of callbackfn with undefined as the this value and argument list containing accumulator, kValue, k, and O.
        var accumulator = @Call(callbackfn,undefined,[accumulator,kValue,k,O]);
      }
      // d. Increase k by 1.
      k += 1;
    }
    // 10. Return accumulator.
    return accumulator;
  }
  