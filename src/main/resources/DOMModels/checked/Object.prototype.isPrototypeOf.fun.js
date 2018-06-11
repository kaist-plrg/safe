function isPrototypeOf (V) {
    // 1. If V is not an object, return false.
    if (typeof V !== 'function' && typeof V !== 'object' || V === null) return false;
    // 2. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 3. Repeat
    var _V = V;
    while (true) {
      // a. Let V be the value of the [[Prototype]] internal property of V.
      V = @Prototype(V);
      // b. if V is null, return false
      if (V === null) return false;
      // c. If O and V refer to the same object, return true.
      if (O === V) return true;
    }
  }
  