function setUTCMilliseconds (ms) {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. Let time be MakeTime(HourFromTime(t), MinFromTime(t), SecFromTime(t), ToNumber(ms)).
    var arg1;
    var arg2;
    var arg3;
    var arg4;
    // HourFromTime(t)
    var HoursPerDay = 24;
    var msPerHour = 3600000;
    var HourFromTime_t = @floor(t / msPerHour) % HoursPerDay;
    arg1 = HourFromTime_t;
    // MinFromTime(t)
    var MinutesPerHour = 60;
    var msPerMinute = 60000;
    var MinFromTime_t = @floor(t / msPerMinute) % MinutesPerHour;
    arg2 = MinFromTime_t;
    // SecFromTime(t)
    var SecondsPerMinute= 60;
    var msPerSecond = 1000;
    var SecFromTime_t  = @floor(t / msPerSecond) % SecondsPerMinute;
    arg3 = SecFromTime_t;
    arg4 = @ToNumber(ms);
    // MakeTime
    var hour = arg1;
    var min = arg2;
    var sec = arg3;
    var ms = arg4;
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
    var time = return_val;
    // 3. Let v be TimeClip(MakeDate(Day(t), time)).
    var arg1;
    var arg2 = time;
    var arg3;
    // Day(t)
    var t = @PrimitiveValue(this);
    var msPerDay = 86400000;
    var Day_t = @floor(t/msPerDay);
    arg1 = Day_t;
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
    arg3 = return_val;
    // TimeClip
    var time = arg3;
    var return_val;
    if (time === Infinity) {
      return_val = NaN;
    }
    else if (@abs(time) > 8.64e+15) {
      return_val = NaN;
    }else {
      var v = return_val;
    }
    // 4. Set the [[PrimitiveValue]] internal property of this Date object to v.
    @PrimitiveValue(this,v);
    // 5. Return v.
    return v;
  }
  