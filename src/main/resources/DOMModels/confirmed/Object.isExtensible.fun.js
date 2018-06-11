function isExtensible (O) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. Return the Boolean value of the [[Extensible]] internal property of O.
    return @Extensible(O);
  }
  