function shift () {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 3. Let len be ToUint32(lenVal).
    var len = @ToUint32(lenVal);
    // 4. If len is zero, then
    if(@SameValue(len,0)){
      // a. Call the [[Put]] internal method of O with arguments "length", 0, and true.
      O.length = 0;
      // b. Return undefined.
      return undefined;
    }
    // 5. Let first be the result of calling the [[Get]] internal method of O with argument "0".
    var first = O[0];
    // 6. Let k be 1.
    var k = 1;
    // 7. Repeat, while k < len
    while (k < len) {
      // a. Let from be ToString(k).
      var from = @ToString(k);
      // b. Let to be ToString(k–1).
      var to = @ToString(k-1);
      // c. Let fromPresent be the result of calling the [[HasProperty]] internal method of O with argument from.
      var fromPresent = (from in O);
      // d. If fromPresent is true, then
      if(fromPresent){
        // i. Let fromVal be the result of calling the [[Get]] internal method of O with argument from.
        var fromVal = O[from];
        // ii. Call the [[Put]] internal method of O with arguments to, fromVal, and true.
        O[to] = fromVal;
      }
      // e. Else, fromPresent is false
      else{
        // i. Call the [[Delete]] internal method of O with arguments to and true.
        delete O[to];
      }
      // f. Increase k by 1.
      k += 1;
    }
    // 8. Call the [[Delete]] internal method of O with arguments ToString(len–1) and true.
    delete O[@ToString(len-1)];
    // 9. Call the [[Put]] internal method of O with arguments "length", (len–1) , and true.
    O.length = len-1;
    // 10. Return first.
    return first;
  }
  