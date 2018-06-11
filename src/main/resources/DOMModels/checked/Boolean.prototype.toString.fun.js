function toString () {
    // 1. Let B be the this value.
    var B = this;
    var b;
    // 2. If Type(B) is Boolean, then let b be B.
    if (typeof B === "Boolean") b = B;
    // 3. Else if Type(B) is Object and the value of the [[Class]] internal property of B is "Boolean",
    //    then let b be the value of the [[PrimitiveValue]] internal property of B.
    else if (typeof B === "object" && B !== null && @Class(B) === "Boolean") b = @PrimitiveValue(B);
    // 4. Else throw a TypeError exception.
    else throw new TypeError();
    // 5. If b is true, then return "true"; else return "false".
    if (b) return "true";
    else return "false";
  }
  