function seal (O) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. For each named own property name P of O,
    var names = @getOwnPropertyNames(O)
    for (var i = 0; i < names.length; i++) {
      var P = names[i];
      // a. Let desc be the result of calling the [[GetOwnProperty]] internal method of O with P.
      var desc = @GetOwnProperty(O, P);
      // b. If desc.[[Configurable]] is true, set desc.[[Configurable]] to false.
      if (desc.configurable) desc.configurable = false;
      // c. Call the [[DefineOwnProperty]] internal method of O with P, desc, and true as arguments.
      @DefineOwnProperty(O, P, desc);
    }
    // 3. Set the [[Extensible]] internal property of O to false.
    @Extensible(O, false);
    // 4. Return O.
    return O;
  }
  