function keys (O) {
    // 1. If the Type(O) is not Object, throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. Let n be the number of own enumerable properties of O
    var names = @getOwnPropertyNames(O);
    // 3. Let array be the result of creating a new Object as if by the expression new Array(n) where Array is
    //    the standard built-in constructor with that name.
    var array = [];
    var n = 0;
    // 4. Let index be 0.
    // 5. For each own enumerable property of O whose name String is P
    // a. Call the [[DefineOwnProperty]] internal method of array with arguments ToString(index), the PropertyDescriptor {[[Value]]: P, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false.
    // b. Increment index by 1.
    for (var i = 0; i < names.length; i++) {
      var P = names[i];
      var desc = @GetOwnProperty(O, P);
      if (desc.enumerable) {
        @DefineOwnProperty(array, @ToString(n), {
          value: P,
          writable: true,
          enumerable: true,
          configurable: true
        });
        n++;
      }
    }
    array.length = n;
    // 6. Return array.
    return array;
  }
  