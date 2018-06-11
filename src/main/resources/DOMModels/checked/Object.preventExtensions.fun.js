function preventExtensions (O) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. Set the [[Extensible]] internal property of O to false.
    @Extensible(O, false);
    // 3. Return O.
    return O;
  }
  