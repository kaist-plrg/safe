function hasOwnProperty (V) {
    // 1. Let P be ToString(V).
    var P = @ToString(V);
    // 2. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 3. Let desc be the result of calling the [[GetOwnProperty]] internal method of O passing P as the argument.
    var desc = @GetOwnProperty(O, P);
    // 4. If desc is undefined, return false.
    if (desc === undefined) return false;
    // 5. Return true.
    return true;
  }
  