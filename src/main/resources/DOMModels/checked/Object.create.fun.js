function create (O, Properties) {
    // 1. If Type(O) is not Object or Null throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object') throw new TypeError();
    // 2. Let obj be the result of creating a new object as if by the expression new Object() where Object is the
    // standard built-in constructor with that name
    var obj = new Object();
    // 3. Set the [[Prototype]] internal property of obj to O.
    @Prototype(obj, O);
    // 4. If the argument Properties is present and not undefined, add own properties to obj as if by calling the
    // standard built-in function Object.defineProperties with arguments obj and Properties.
    if (Properties !== undefined) Object.defineProperties(obj, Properties);
    // 5. Return obj.
    return obj;
  }
  