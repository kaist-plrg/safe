function forEach (callbackfn, thisArg) {
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
    // 5. If thisArg was supplied, let T be thisArg; else let T be undefined.
    if (arguments.length > 1){
      var T = thisArg;
    }else {
      var T = undefined;
    }
    // 6. Let k be 0.
    var k = 0;
    // 7. Repeat, while k < len
    while (k < len) {
      // a. Let Pk be ToString(k).
      var Pk = @ToString(k);
      // b. Let kPresent be the result of calling the [[HasProperty]] internal method of O with argument Pk.
      var kPresent = (Pk in O);
      // c. If kPresent is true, then
      if (kPresent) {
        // i. Let kValue be the result of calling the [[Get]] internal method of O with argument Pk.
        var kValue = O[Pk];
        // ii. Call the [[Call]] internal method of callbackfn with T as the this value and argument list containing kValue, k, and O.
        @Call(callbackfn,T,[kValue,k,O])
      }
      // d. Increase k by 1.
      k += 1;
    }
    // 8. Return undefined.
    return undefined;
  }
  