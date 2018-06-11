function toString () {
    // it throws a TypeError exception if its this value is not a String or a String object.
    if (typeof this === "string") return this;
    else if (typeof this === "object" && this !== null && @Class(this) === "String") return @PrimitiveValue(this);
    else throw new TypeError();
  }
  