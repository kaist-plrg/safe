function getUTCSeconds () {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. If t is NaN, return NaN.
    if (@SameValue(t,NaN)) {
      return NaN;
    }
    // 3. Return SecFromTime(t).
    var SecondsPerMinute= 60;
    var msPerSecond = 1000;
    var SecFromTime_t  = @floor(t / msPerSecond) % SecondsPerMinute;
    return SecFromTime_t;
  }
  