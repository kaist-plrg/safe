function toISOString () {
		// TypeError is thrown when this is any other objects instead of Date object
		if (!(this instanceof Date)) throw new TypeError();
    // Need to know internal structure of Date object
    var t = @PrimitiveValue(this);
    if (@SameValue(t,Infinity) || @SameValue(t,-Infinity) || @SameValue(t,NaN)) {
      throw new RangeError();
    }
    return @StrTop;
  }
  