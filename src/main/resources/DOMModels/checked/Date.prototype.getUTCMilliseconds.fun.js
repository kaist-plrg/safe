function getUTCMilliseconds () {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. If t is NaN, return NaN.
    if (@SameValue(t,NaN)) {
      return NaN;
    }
    // 3. Return msFromTime(t).
    var msPerSecond = 1000;
    var msFromTime_t = t % msPerSecond;
    return msFromTime_t;
  }
  