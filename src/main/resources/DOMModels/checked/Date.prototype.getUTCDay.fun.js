function getUTCDay () {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. If t is NaN, return NaN.
    if (@SameValue(t,NaN)) {
      return NaN;
    }
    // 3. Return WeekDay(t).
    var msPerDay = 86400000;
    var Day_t = @floor(t/msPerDay);

    var WeekDay_t = (Day_t + 4) % 7;
    return WeekDay_t;
  }
  