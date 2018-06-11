function slice (start, end) {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let A be a new array created as if by the expression new Array() where Array is the standard built-in constructor with that name.
    var A = new Array();
    // 3. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 4. Let len be ToUint32(lenVal).
    var len = @ToUint32(lenVal);
    // 5. Let relativeStart be ToInteger(start).
    var relativeStart = @ToInteger(start);
    // 6. If relativeStart is negative, let k be max((len + relativeStart),0); else let k be           min(relativeStart, len).
    if(relativeStart<0){
      var k = Math.max(len+relativeStart,0);
    }else{
      var k = Math.min(relativeStart,len);
    }
    // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be ToInteger(end).
    if(@SameValue(end,undefined)){
      var relativeEnd = len;
    }else{
      var relativeEnd = @ToInteger(end);
    }
    // 8. If relativeEnd is negative, let final be max((len + relativeEnd),0); else let final be       min(relativeEnd, len).
    if(relativeEnd<0){
      var final = Math.max(len+relativeEnd,0);
    }else{
      var final = Math.min(relativeEnd,len);
    }
    // 9. Let n be 0.
    var n = 0;
    // 10. Repeat, while k < final
    while(k<final){
      // a. Let Pk be ToString(k).
      var Pk = @ToString(k);
      // b. Let kPresent be the result of calling the [[HasProperty]] internal method of O with        argument Pk.
      var kPresent = Pk in O;
      // c. If kPresent is true, then
      if(kPresent){
        // i. Let kValue be the result of calling the [[Get]] internal method of O with argument Pk.
        var kValue = O[Pk];
        // ii. Call the [[DefineOwnProperty]] internal method of A with arguments ToString(n),         Property Descriptor {[[Value]]: kValue, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]:    true}, and false.
        @DefineOwnProperty(A,@ToString(n),{value:kValue,writable:true,enumerable:true,configurable:    true});
      }
      // d. Increase k by 1.
      k += 1;
      // e. Increase n by 1.
      n += 1;
    }
    A.length = n;
    // 11. Return A.
    return A;
  }
  