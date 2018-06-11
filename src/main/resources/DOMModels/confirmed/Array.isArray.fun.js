function isArray (arg) {
    // 1. If Type(arg) is not Object, return false.
    if (typeof arg !== "object" || arg === null) return false;
    // 2. If the value of the [[Class]] internal property of arg is "Array", then return true.
    if (@SameValue(@Class(arg), "Array")) return true;
    // 3. Return false.
    return false;
  }
  