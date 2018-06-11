function push () {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let lenVal be the result of calling the [[Get]] internal method of O with argument "length".
    var lenVal = O.length;
    // 3. Let n be ToUint32(lenVal).
    var n = @ToUint32(lenVal);
    // 4. Let items be an internal List whose elements are, in left to right order,
    // the arguments that were passed to this function invocation.
    var items = arguments;
    // 5. Repeat, while items is not empty
    for (var i = 0; i < items.length; i++) {
      // a. Remove the first element from items and let E be the value of the element.
      var E = items[i];
      // b. Call the [[Put]] internal method of O with arguments ToString(n), E, and true.
      O[@ToString(n)] = E
      // c. Increase n by 1.
      n++;
    }
    // 6. Call the [[Put]] internal method of O with arguments "length", n, and true.
    O.length = n
    // 7. Return n.
    return n;
  }
  