function isFrozen (O) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. For each named own property name P of O,
    var names = @getOwnPropertyNames(O);
    for (var i = 0; i < names.length; i++) {
      var P = names[i];
      // a. Let desc be the result of calling the [[GetOwnProperty]] internal method of O with P.
      var desc = @GetOwnProperty(O, P);
      // b. If IsDataDescriptor(desc) is true then
      // i. If desc.[[Writable]] is true, return false. c. If desc.[[Configurable]] is true, then return false.
      if (desc.writable || desc.configurable) return false;
    }
    // 3. If the [[Extensible]] internal property of O is false, then return true.
    if (@Extensible(O) === false) return true;
    // 4. Otherwise, return false.
    else return false;
  }
  