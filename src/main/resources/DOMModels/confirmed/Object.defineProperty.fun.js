function defineProperty (O, P, Attributes) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. Let name be ToString(P).
    var name = @ToString(P);
    // 3. Let desc be the result of calling ToPropertyDescriptor with Attributes as the argument.
    // 4. Call the [[DefineOwnProperty]] internal method of O with arguments name, desc, and true.
    @DefineOwnProperty(O, name, Attributes);
    // 5. Return O.
    return O;
  }
  