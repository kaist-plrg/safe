function toString() {
    // 1. Let array be the result of calling ToObject on the this value.
    var array = @ToObject(this);
    // 2. Let func be the result of calling the [[Get]] internal method of array with argument "join".
    var func = array.join;
    // 3. If IsCallable(func) is false, then let func be the standard built-in method Object.prototype.toString (15.2.4.2).
    if (!@IsCallable(func)) func = Object.prototype.toString;
    // 4. Return the result of calling the [[Call]] internal method of func providing array as the this value and an empty arguments list.
    return @Call(func, array, []);
  }
  