function Date () {
    if (arguments.length === 0) {
      var obj = {};
      @Prototype(obj, Date.prototype);
      @Class(obj, "Date");
      @Extensible(obj, true);
      @PrimitiveValue(obj, @NumTop);
      return obj;
    }else if (arguments.length === 1) {
      var value = arguments[0];
      // 1. Let v be ToPrimitive(value).
      var v = @ToPrimitive(value);
      // 2. If Type(v) is String, then
      if (@SameValue(typeof v,"string")) {
        // a. Parse v as a date, in exactly the same manner as for the parse method (15.9.4.2); let V be the time value for this date.
        var V = Date.parse(v)
      }
      // 3. Else, let V be ToNumber(v).
      else {
        var V = @ToNumber(v);
      }
      // 4. Set the [[PrimitiveValue]] internal property of the newly constructed object to TimeClip(V) and return.
      var time = V;
      var return_val;
      if (time === Infinity) {
        return_val = NaN;
      }
      else if (@abs(time) > 8.64e+15) {
        return_val = NaN;
      }else {
        return_val = @ToInteger(time);
      }
      var obj = {};
      @Prototype(obj, Date.prototype);
      @Class(obj, "Date");
      @Extensible(obj, true);
      @PrimitiveValue(obj, return_val);
      return obj;
    }else if (arguments.length > 1) {
      var year = arguments[0];
      var month = arguments[1];
      // 1. Let y be ToNumber(year).
      var y = @ToNumber(year);
      // 2. Let m be ToNumber(month).
      var m = @ToNumber(month);
      // 3. If date is supplied then let dt be ToNumber(date); else let dt be 1.
      if (arguments.length > 2) {
        var dt = @ToNumber(arguments[2]);
      }else {
        var dt = 1;
      }
      // 4. If hours is supplied then let h be ToNumber(hours); else let h be 0.
      if (arguments.length > 3) {
        var h = @ToNumber(arguments[3]);
      }else {
        var h = 0;
      }
      // 5. If minutes is supplied then let min be ToNumber(minutes); else let min be 0.
      if (arguments.length > 4) {
        var min = @ToNumber(arguments[4]);
      }else {
        var min = 0;
      }
      // 6. If seconds is supplied then let s be ToNumber(seconds); else let s be 0.
      if (arguments.length > 5) {
        var s = @ToNumber(arguments[5]);
      }else {
        var s = 0;
      }
      // 7. If ms is supplied then let milli be ToNumber(ms); else let milli be 0.
      if (arguments.length > 6) {
        var milli = @ToNumber(arguments[6]);
      }else {
        milli = 0;
      }
      // 8. If y is not NaN and 0   ToInteger(y)   99, then let yr be 1900+ToInteger(y); otherwise, let yr be y.
      if (!@SameValue(y,NaN) && (0 <= @ToInteger(y) && @ToInteger(y) <= 99)) {
        var yr = 1900 + @ToInteger(t);
      }else {
        var yr = y;
      }
      // 9. Let finalDate be MakeDate(MakeDay(yr, m, dt), MakeTime(h, min, s, milli)).
      var arg1;
      var arg2;
      // MakeDay
      var year = yr;
      var month = m;
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
        }
        else {
        var msPerDay = 86400000;
        var Day_t = @floor(t/msPerDay);
        return_val = Day_t + dt - 1;
        }
      }
      arg1 = return_val;
      //  MakeTime
      var hour = h;
      var sec = s;
      var ms = milli;
      var return_val;

      var HoursPerDay = 24;
      var MinutesPerHour = 60;
      var SecondsPerMinute= 60;
      var msPerSecond = 1000;
      var msPerMinute = 60000;
      var msPerHour = 3600000;


      if (hour === Infinity || min === Infinity || sec === Infinity || ms === Infinity) {
        return_val = NaN;
      }else {
        var h = @ToInteger(hour);
        var m = @ToInteger(min);
        var s = @ToInteger(sec);
        var milli = @ToInteger(ms);
        var t = h * msPerHour + m * msPerMinute + s * msPerSecond + milli;
        return_val = t;
      }
      arg2 = return_val;
      // MakeDate
      var day = arg1;
      var time = arg2;
      var return_val;
      var msPerDay = 86400000;
      if (day === Infinity || time === Infinity) {
        return_val = NaN;
      }else {
        return_val = day * msPerDay + time;
      }
      var finalDate = return_val;
      // 10. Set the [[PrimitiveValue]] internal property of the newly constructed object to TimeClip(UTC(finalDate)
      // TODO Need UTC(t) -> Need LocalTZA
      var time = finalDate;
      var return_val;
      if (time === Infinity || time === -Infinity || @SameValue(time,NaN)) {
        return_val = NaN;
      }else if (@abs(time) > 8.64e+15) {
        return_val = NaN;
      }else {
        return_val = @ToInteger(time);
      }
      var obj = {};
      @Prototype(obj, Date.prototype);
      @Class(obj, "Date");
      @Extensible(obj, true);
      @PrimitiveValue(obj, return_val);
      return obj;
    }
  }
  