function getUTCMinutes () {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. If t is NaN, return NaN.
    if (@SameValue(t,NaN)) {
      return NaN;
    }
    // 3. Return MinFromTime(t).
    var MinutesPerHour = 60;
    var msPerMinute = 60000;
    var MinFromTime_t = @floor(t / msPerMinute) % MinutesPerHour;
    return MinFromTime_t;
  }
  