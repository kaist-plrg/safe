function getUTCHours () {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. If t is NaN, return NaN.
    if (@SameValue(t,NaN)) {
      return NaN;
    }
    // 3. Return HourFromTime(t).
    var HoursPerDay = 24;
    var msPerHour = 3600000;
    var HourFromTime_t = @floor(t / msPerHour) % HoursPerDay;
    return HourFromTime_t;
  }
  