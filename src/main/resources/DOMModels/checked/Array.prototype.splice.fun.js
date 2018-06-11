function splice (start, deleteCount) {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let A be a new array created as if by the expression new Array()where Array is the standard  built-in constructor with that name.
    var A = new Array();
    // 3. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 4. Let len be ToUint32(lenVal).
    var len = @ToUint32(lenVal);
    // 5. Let relativeStart be ToInteger(start).
    var relativeStart = @ToInteger(start);
    // 6. If relativeStart is negative, let actualStart be max((len + relativeStart),0); else let actualStart be min(relativeStart, len).
    if (relativeStart < 0) {
      var actualStart = Math.max(len + relativeStart, 0);
    }else {
      var actualStart = Math.min(relativeStart, len);
    }
    // 7. Let actualDeleteCount be min(max(ToInteger(deleteCount),0), len – actualStart).
    var actualDeleteCount = Math.min(Math.max(@ToInteger(deleteCount), 0),len - actualStart);
    // 8. Let k be 0.
    var k = 0;
    // 9. Repeat, while k < actualDeleteCount
    while (k < actualDeleteCount) {
      // a. Let from be ToString(actualStart+k).
      var from = @ToString(actualStart + k);
      // b. Let fromPresent be the result of calling the [[HasProperty]] internal method of O with     argument from.
      var fromPresent = (from in O);
      // c. If fromPresent is true, then
      if (fromPresent) {
        // i. Let fromValue be the result of calling the [[Get]] internal method of O with argument    from.
        var fromValue = O[from];
        // ii. Call the [[DefineOwnProperty]] internal method of A with arguments ToString(k),         Property Descriptor {[[Value]]: fromValue, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false.
        @DefineOwnProperty(A,@ToString(k),{value : fromValue, writable : true, enumerable : true,      configurable : true});
      }
      // d. Increment k by 1.
      k += 1;
    }
    // 10. Let items be an internal List whose elements are, in left to right order, the portion of    the actual argument list starting with item1. The list will be empty if no such items are present.
    items = new Array();
    for (var i = 0;i < arguments.length - 2;i++) {
      items[i] = arguments[i+2];
    }
    // 11. Let itemCount be the number of elements in items.
    var itemCount = items.length;
    // 12. If itemCount < actualDeleteCount, then
    if ( itemCount < actualDeleteCount) {
      // a. Let k be actualStart.
      var k = actualStart;
      // b. Repeat, while k < (len – actualDeleteCount)
      while ( k < (len - actualDeleteCount)) {
        // i. Let from be ToString(k+actualDeleteCount).
        var from = @ToString(k + actualDeleteCount);
        // ii. Let to be ToString(k+itemCount).
        var to = @ToString(k + itemCount);
        // iii. Let fromPresent be the result of calling the [[HasProperty]] internal method of O with argument from.
        var fromPresent = (from in O);
        // iv If fromPresent is true, then
        if (fromPresent) {
          // 1. Let fromValue be the result of calling the [[Get]] internal method of O with argument  from.
          var fromValue = O[from];
          // 2. Call the [[Put]] internal method of O with arguments to, fromValue, and true.
          O[to] = fromValue;
        }
        // v. Else, fromPresent is false
        else {
          // 1. Call the [[Delete]] internal method of O with arguments to and true.
          delete O[to];
        }
        // vi. Increase k by 1.
        k += 1;
      }
      //c. Let k be len.
      var k = len;
      // d. Repeat, while k > (len – actualDeleteCount + itemCount)
      while ( k > (len - actualDeleteCount + itemCount)) {
        // i. Call the [[Delete]] internal method of O with arguments ToString(k–1) and true.
       delete O[@ToString(k-1)];
       // ii. Decrease k by 1.
       k -= 1;
      }
    }
    // 13. Else if itemCount > actualDeleteCount, then
    else {
      // a. Let k be (len – actualDeleteCount).
      var k = len - actualDeleteCount;
      // b. Repeat, while k > actualStart
      while ( k > actualStart) {
        // i. Let from be ToString(k + actualDeleteCount – 1).
        var from = @ToString(k + actualDeleteCount - 1);
        // ii. Let to be ToString(k + itemCount – 1)
        var to = @ToString(k + itemCount - 1);
        // iii. Let fromPresent be the result of calling the [[HasProperty]] internal method of O with argument from.
        var fromPresent = (from in O);
        // iv. If fromPresent is true, then
        if (fromPresent) {
          // 1. Let fromValue be the result of calling the [[Get]] internal method of O with argument  from.
          var fromValue = O[from];
          // 2. Call the [[Put]] internal method of O with arguments to, fromValue, and true.
          O[to] = fromValue;
        }
        // v. Else, fromPresent is false
        else {
          // 1. Call the [[Delete]] internal method of O with argument to and true.
          delete O[to];
        }
        // vi. Decrease k by 1.
        k -= 1;
      }
    }
    // 14. Let k be actualStart.
    var k = actualStart;
    // 15. Repeat, while items is not empty
    for (var i = 0; i < items.length; i++) {
      // a. Remove the first element from items and let E be the value of that element.
      var E = items[i];
      // b. Call the [[Put]] internal method of O with arguments ToString(k), E, and true.
      O[@ToString(k)] = E;
      // c. Increase k by 1.
      k += 1;
    }
    // 16. Call the [[Put]] internal method of O with arguments "length", (len – actualDeleteCount +   itemCount), and true.
    O.length = len - actualDeleteCount + itemCount;
    A.length = actualDeleteCount;
    // 17. Return A.
    return A;
  }
  