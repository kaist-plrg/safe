function valueOf () {
    // The valueOf function is not generic; it throws a TypeError exception if its this value is not a Number or a Number object. Therefore, it cannot be transferred to other kinds of objects for use as a method.
    if (typeof this === "number") return this;
    else if (typeof this === "object" && this !== null && @Class(this) === "Number") return @PrimitiveValue(this);
    else throw new TypeError();
  }
  