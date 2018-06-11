function lastIndexOf (searchElement,fromIndex) {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenValue be the result of calling the [[Get]] internal method of O with the argument        "length".
    var lenValue = O.length;
    // 3. Let len be ToUint32(lenValue).
    var len = @ToUint32(lenValue);
    // 4. If len is 0, return -1.
    if (@SameValue(len,0)) {
      return -1;
    }
    // 5. If argument fromIndex was passed let n be ToInteger(fromIndex); else let n be len-1.
    if(arguments.length>1){
      var n = @ToInteger(fromIndex);
    }else {
      var n = len-1;
    }
    // 6. If n ≥ 0, then let k be min(n,len–1).
    if (n>=0){
      var k = Math.min(n,len-1);
    }
    // 7. Else,n<0
    else {
      // a. Let k be len - abs(n).
      var k = len - @abs(n);
    }
    // 8. Repeat, while k≥ 0
    while (k>=0) {
      // a. Let kPresent be the result of calling the [[HasProperty]] internal method of O with argument  ToString(k).
      var kPresent = (@ToString(k) in O);
      // b. If kPresent is true, then
      if (kPresent) {
        // i. Let elementK be the result of calling the [[Get]] internal method of O with the argument    ToString(k).
        var elementK = O[@ToString(k)];
        // ii. Let same be the result of applying the Strict Equality Comparison Algorithm to             searchElement and elementK.
        var same = (searchElement === elementK);
        // iii. If same is true, return k.
        if (same) {
          return k;
        }
      }
      // c. Decrease k by 1.
      k -= 1;
    }
    // 9. Return -1.
    return -1;
  }
  