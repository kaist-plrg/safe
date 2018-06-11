function setUTCDate (date) {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. Let dt be ToNumber(date).
    var dt = @ToNumber(date);
    // 3. Let newDate be MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), dt), TimeWithinDay(t)).
    var arg1;
    var arg2;
    var arg3;
    var arg4;
    // YearFromTime(t);
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
    arg1 = YearFromTime_t;
    // MonthFromTime(t)
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
    arg2 = MonthFromTime_t;
    // TimeWithinDay(t)
    var msPerDay = 86400000;
    var TimeWithinDay_t = t % msPerDay;
    arg3 = TimeWithinDay_t;
    // MakeDay
    var year = arg1;
    var month = arg2;
    var date = dt;
    var return_val;
    if (year === Infinity || month === Infinity || date === Infinity) {
      return_val = NaN;
    }else {
      var y = @ToInteger(year);
      var m = @ToInteger(month);
      var dt = @ToInteger(date);
      var ym = y + @floor(m/12);
      var mn = m % 12;
      var t = 0;
      while (t < 8.64e+15) {
        // YearFromTime(t)
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
        if (YearFromTime_t === ym) {
          // MonthFromTime(t)
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
          if (MonthFromTime_t === mn) {
            // DateFromTime(t)
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

            if (MonthFromTime_t === 0) {
              var DateFromTime = DayWithinYear_t + 1;
            }else if (MonthFromTime_t === 1) {
              var DateFromTime = DayWithinYear_t - 30;
            }else if (MonthFromTime_t === 2) {
              var DateFromTime = DayWithinYear_t - 58 - InLeapYear_t;
            }else if (MonthFromTime_t === 3) {
              var DateFromTime = DayWithinYear_t - 89 - InLeapYear_t;
            }else if (MonthFromTime_t === 4) {
              var DateFromTime = DayWithinYear_t - 119 - InLeapYear_t;
            }else if (MonthFromTime_t === 5) {
              var DateFromTime = DayWithinYear_t - 150 - InLeapYear_t;
            }else if (MonthFromTime_t === 6) {
              var DateFromTime = DayWithinYear_t - 180 - InLeapYear_t;
            }else if (MonthFromTime_t === 7) {
              var DateFromTime = DayWithinYear_t - 211 - InLeapYear_t;
            }else if (MonthFromTime_t === 8) {
              var DateFromTime = DayWithinYear_t - 242 - InLeapYear_t;
            }else if (MonthFromTime_t === 9) {
              var DateFromTime = DayWithinYear_t - 272 - InLeapYear_t;
            }else if (MonthFromTime_t === 10) {
              var DateFromTime = DayWithinYear_t - 303 - InLeapYear_t;
            }else if (MonthFromTime_t === 11) {
              var DateFromTime = DayWithinYear_t - 333 - InLeapYear_t;
            }
            if (DateFromTime === 1){
              break;
            }
          }
        }
        t += 1;
      }
      if (t === 8.64e+15) {
        return_val = NaN;
      }else {
      var msPerDay = 86400000;
      var Day_t = @floor(t/msPerDay);
      return_val = Day_t + dt - 1;
      }
    }
    arg4 = return_val;
    // Makedate
    var day = arg4;
    var time = arg3;
    var return_val;
    var msPerDay = 86400000;
    if (day === Infinity || time === Infinity) {
      return_val = NaN;
    }else {
      return_val = day * msPerDay + time;
    }
    var newDate = return_val;
    // 4. Let v be TimeClip(newDate).
    var time = newDate;
    var return_val;
    if (time === Infinity) {
      return_val = NaN;
    }
    else if (@abs(time) > 8.64e+15) {
      return_val = NaN;
    }else {
      return_val = @ToInteger(time);
    }
    var v = return_val;
    // 5. Set the [[PrimitiveValue]] internal property of this Date object to v.
    @PrimitiveValue(this,v);
    // 6. Return v.
    return v;
  }
  