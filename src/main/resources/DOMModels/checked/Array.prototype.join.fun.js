function join (separator) {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 3. Let len be ToUint32(lenVal).
    var len = @ToUint32(lenVal);
    // 4. If separator is undefined, let separator be the single-character String ",".
    if (separator === undefined){
        var separator = ",";
    }
    // 5. Let sep be ToString(separator).
    var sep = @ToString(separator);
    // 6. If len is zero, return the empty String.
    if (len === 0){
        return "";
    }
    // 7. Let element0 be the result of calling the [[Get]] internal method of O with argument "0".
    var element0 = O[0];
    // 8. If element0 is undefined or null, let R be the empty String; otherwise, Let R be ToString(element0).
    if (@SameValue(element0,undefined) || @SameValue(element0,null)){
        var R = "";
    }else {
        var R = @ToString(element0);
    }
    // 9. Let k be 1.
    var k = 1;
    // 10. Repeat, while k < len
    while (k < len) {
      // a. Let S be the String value produced by concatenating R and sep.
      var S = R + sep;
      // b. Let element be the result of calling the [[Get]] internal method of O with argument ToString(k).
      var element = O[@ToString(k)];
      // c. If element is undefined or null, Let next be the empty String; otherwise, let next be ToString(element).
      if (@SameValue(element,undefined) || @SameValue(element,null)) {
        var next = "";
      }else {
        var next = @ToString(element);
      }
      // d. Let R be a String value produced by concatenating S and next.
      var R = S + next;
      // e. Increase k by 1.
      k += 1;
    }
    //11. Return R.
    return R;
  }
  