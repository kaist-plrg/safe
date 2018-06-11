function indexOf (searchElement,fromIndex) {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenValue be the result of calling the [[Get]] internal method of O with the argument "length".
    var lenValue = O.length;
    // 3. Let len be ToUint32(lenValue).
    var len = @ToUint32(lenValue);
    // 4. If len is 0, return -1.
    if (@SameValue(len,0)){
      return -1;
    }
    // 5. If argument fromIndex was passed let n be ToInteger(fromIndex); else let n be 0.
    if (arguments.length >1){
      var n = @ToInteger(fromIndex);
    }else{
      var n = 0;
    }
    // 6. If n ≥ len, return -1.
    if (n>=len){
      return -1;
    }
    // 7. Ifn≥0,then
    if (n>=0){
      // a. Let k be n.
      var k = n;
    }
    // 8. Else, n<0
    else{
      // a. Let k be len - abs(n).
      var k = len - @abs(n);
      // b. If k is less than 0,then let k be 0.
      if (k<0){
        var k = 0;
      }
    }
    // 9. Repeat, while k<len
    while (k<len){
      // a. Let kPresent be the result of calling the [[HasProperty]] internal method of O with argument ToString(k).
      var kPresent = (@ToString(k) in O);
      // b. If kPresent is true, then
      if (kPresent){
        // i. Let elementK be the result of calling the [[Get]] internal method of O with the argument ToString(k).
        var elementK = O[@ToString(k)];
        // ii. Let same be the result of applying the Strict Equality Comparison Algorithm to searchElement and elementK.
        var same = (searchElement === elementK);
        // iii. If same is true, return k.
        if (same) {
          return k;
        }
      }
      // c. Increase k by 1.
      k +=1;
    }
    // 10. Return -1.
    return -1;
  }
  