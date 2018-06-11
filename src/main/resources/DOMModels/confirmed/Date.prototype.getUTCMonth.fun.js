function getUTCMonth () {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. If t is NaN, return NaN.
    if (@SameValue(t,NaN)) {
      return NaN;
    }
    // 3. Return MonthFromTime(t).
    var msPerDay = 86400000;
    var Day_t = @floor(t/msPerDay);

    var y = 0;
    var DayFromYear_y = 365 * (y - 1970) + @floor((y - 1969) / 4) - @floor((y - 1901) / 100) + @floor((y - 1601) / 400);

    var TimeFromYear_y = msPerDay * DayFromYear_y;

    while (TimeFromYear_y <= t) {
      y += 1;
      var DayFromYear_y = 365 * (y - 1970) + @floor((y - 1969) / 4) - @floor((y - 1901) / 100) + @floor((y - 1601) / 400);

      var TimeFromYear_y = msPerDay * DayFromYear_y;
    }
    y -= 1;
    var YearFromTime_t = y;

    var y = YearFromTime_t;

    var DayFromYear_y = 365 * (y - 1970) + @floor((y - 1969) / 4) - @floor((y - 1901) / 100) + @floor((y - 1601) / 400);

    var DayWithinYear_t = Day_t - DayFromYear_y

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

    var y = YearFromTime_t;

    if (y % 4 !== 0) {
      var DaysInYear_y = 365;
    }else if (y % 4 === 0 && y % 100 !== 0) {
      var DaysInYear_y = 366;
    }else if (y % 100 === 0 && y % 400 !== 0) {
      var DaysInYear_y = 365;
    }else if (y % 400 === 0) {
      var DaysInYear_y = 366;
    }

    if (DaysInYear_y === 365) {
      var InLeapYear_t = 0;
    }else {
      var InLeapYear_t = 1;
    }

    if (0 <= DayWithinYear_t < 31) {
      var MonthFromTime_t = 0;
    }else if (31 <= DayWithinYear_t < 59 + InLeapYear_t) {
      var MonthFromTime_t = 1;
    }else if (59 + InLeapYear_t <= DayWithinYear_t < 90 + InLeapYear_t) {
      var MonthFromTime_t = 2;
    }else if (90 + InLeapYear_t <= DayWithinYear_t < 120 + InLeapYear_t) {
      var MonthFromTime_t = 3;
    }else if (120 + InLeapYear_t <= DayWithinYear_t < 151 + InLeapYear_t) {
      var MonthFromTime_t = 4;
    }else if (151 + InLeapYear_t <= DayWithinYear_t < 181 + InLeapYear_t) {
      var MonthFromTime_t = 5;
    }else if (181 + InLeapYear_t <= DayWithinYear_t < 212 + InLeapYear_t) {
      var MonthFromTime_t = 6;
    }else if (212 + InLeapYear_t <= DayWithinYear_t < 243 + InLeapYear_t) {
      var MonthFromTime_t = 7;
    }else if (243 + InLeapYear_t <= DayWithinYear_t < 273 + InLeapYear_t) {
      var MonthFromTime_t = 8;
    }else if (273 + InLeapYear_t <= DayWithinYear_t < 304 + InLeapYear_t) {
      var MonthFromTime_t = 9;
    }else if (304 + InLeapYear_t <= DayWithinYear_t < 334 + InLeapYear_t) {
      var MonthFromTime_t = 10;
    }else if (334 + InLeapYear_t <= DayWithinYear_t < 365 + InLeapYear_t) {
      var MonthFromTime_t = 11;
    }
    return MonthFromTime_t;
  }
  