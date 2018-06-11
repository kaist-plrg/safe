function getOwnPropertyDescriptor (O, P) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. Let name be ToString(P).
    var name = @ToString(P);
    // 3. Let desc be the result of calling the [[GetOwnProperty]] internal method of O with argument name.
    // 4. Return the result of calling FromPropertyDescriptor(desc) (8.10.4).
    return @GetOwnProperty(O, name);
  }
  