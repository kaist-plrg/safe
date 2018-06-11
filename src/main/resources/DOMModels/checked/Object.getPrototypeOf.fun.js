function getPrototypeOf (O) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. Return the value of the [[Prototype]] internal property of O.
    return @Prototype(O);
  }
  