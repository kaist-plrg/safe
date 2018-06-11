function getUTCFullYear () {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. If t is NaN, return NaN.
    if (@SameValue(t,NaN)) {
      return NaN;
    }
    // 3. Return YearFromTime(t).
    var y = 0;
    var DayFromYear_y = 365 * (y - 1970) + @floor((y - 1969) / 4) - @floor((y - 1901) / 100) + @floor((y - 1601) / 400);

    var msPerDay = 86400000;
    var TimeFromYear_y = msPerDay * DayFromYear_y;

    while (TimeFromYear_y <= t) {
      y += 1;
      var DayFromYear_y = 365 * (y - 1970) + @floor((y - 1969) / 4) - @floor((y - 1901) / 100) + @floor((y - 1601) / 400);

      var TimeFromYear_y = msPerDay * DayFromYear_y;
    }
    y -= 1;
    var YearFromTime_t = y;
    return YearFromTime_t;
  }
  