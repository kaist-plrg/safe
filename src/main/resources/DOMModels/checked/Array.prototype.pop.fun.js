function pop () {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 3. Let len be ToUint32(lenVal).
    var len = @ToUint32(lenVal);
    // 4. If len is zero,
    if (@SameValue(len,0)) {
      // a. Call the [[Put]] internal method of O with arguments "length", 0, and true.
      O.length = 0;
      // b. Return undefined.
      return undefined;
    }
    // 5. Else, len > 0
    else {
      // a. Let indx be ToString(len–1).
      var indx = len-1;
      // b. Let element be the result of calling the [[Get]] internal method of O with argument indx.
      var element = O[indx];
      // c. Call the [[Delete]] internal method of O with arguments indx and true.
      delete O[indx];
      // d. Call the [[Put]] internal method of O with arguments "length", indx, and true.
      O.length = indx;
      // e. Return element.
      return element;
    }
  }
  