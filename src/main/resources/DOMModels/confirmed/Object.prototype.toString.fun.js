function toString () {
    // 1. If the this value is undefined, return "[object Undefined]".
    if (this === undefined) return '[object Undefined]';
    // 2. If the this value is null, return "[object Null]".
    if (this === null) return '[object Null]';
    // 3. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 4. Let class be the value of the [[Class]] internal property of O.
    var class = @Class(O);
    // 5. Return the String value that is the result of concatenating the three Strings "[object ", class, and "]"
    return '[object ' + class + ']';
  }
  