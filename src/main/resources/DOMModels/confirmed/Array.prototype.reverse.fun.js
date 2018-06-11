function reverse () {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 3. Let len be ToUint32(lenVal).
    var len = @ToUint32(lenVal);
    // 4. Let middle be floor(len/2).
    var middle = @floor(len/2);
    // 5. Let lower be 0.
    var lower = 0;
    // 6. Repeat, while lower !=  middle
    while(lower != middle){
      // a. Let upper be len-lower-1.
      var upper = len-lower-1;
      // b. Let upperP be ToString(upper).
      var upperP = @ToString(upper);
      // c. Let lowerP be ToString(lower).
      var lowerP = @ToString(lower);
      // d. Let lowerValue be the result of calling the [[Get]] internal method of O with argument lowerP.
      var lowerValue = O[lowerP];
      // e. Let upperValue be the result of calling the [[Get]] internal method of O with argument upperP .
      var upperValue = O[upperP];
      // f. Let lowerExists be the result of calling the [[HasProperty]] internal method of O with argument lowerP.
      var lowerExists = (lowerP in O);
      // g. Let upperExists be the result of calling the [[HasProperty]] internal method of O with argument upperP.
      var upperExists = (upperP in O);
      // h. If lowerExists is true and upperExists is true, then
      if(lowerExists && upperExists){
        // i. Call the [[Put]] internal method of O with arguments lowerP, upperValue, and true .
        O[lowerP] = upperValue;
        // ii. Call the [[Put]] internal method of O with arguments upperP, lowerValue, and true .
        O[upperP] = lowerValue;
      }
      // i. Else if lowerExists is false and upperExists is true, then
      else if(!lowerExists && upperExists){
        // i. Call the [[Put]] internal method of O with arguments lowerP, upperValue, and true .
        O[lowerP] = upperValue;
        // ii. Call the [[Delete]] internal method of O, with arguments upperP and true.
        delete O[upperP];
      }
      // j. Else if lowerExists is true and upperExists is false, then
      else if (lowerExists && !upperExists){
        // i. Call the [[Delete]] internal method of O, with arguments lowerP and true .
        delete O[lowerP];
         // ii. Call the [[Put]] internal method of O with arguments upperP, lowerValue, and true .
         O[upperP] = lowerValue;
      }
      // k. Else both lowerExists and upperExists are false
        // i. No action is required.
      // l. Increase lower by 1.
      lower += 1;
    }
    // 7. Return O .
    return O;
  }
  