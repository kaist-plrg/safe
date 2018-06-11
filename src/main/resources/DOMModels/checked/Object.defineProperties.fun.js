function defineProperties (O, Properties) {
    // 1. If Type(O) is not Object throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 2. Let props be ToObject(Properties).
    var props = @ToObject(Properties);
    // 3. Let names be an internal list containing the names of each enumerable own property of props.
    // 4. Let descriptors be an empty internal List.
    // 5. For each element P of names in list order,
      //   a. Let descObj be the result of calling the [[Get]] internal method of props with P as the argument.
      //   b. Let desc be the result of calling ToPropertyDescriptor with descObj as the argument.
      //   c. Append the pair (a two element List) consisting of P and desc to the end of descriptors.
    // 6. For each pair from descriptors in list order,
    //   a. Let P be the first element of pair.
    //   b. Let desc be the second element of pair.
    //   c. Call the [[DefineOwnProperty]] internal method of O with arguments P, desc, and true.
    var names = @getOwnPropertyNames(props);
    for (var i = 0; i < names.length; i++) {
      var P = names[i];
      var p_desc = @GetOwnProperty(props, P);
      if (p_desc.enumerable) {
        var desc = props[P];
        @DefineOwnProperty(O, P, desc);
      }
    }
    // 7. Return O.
    return O;
  }
  