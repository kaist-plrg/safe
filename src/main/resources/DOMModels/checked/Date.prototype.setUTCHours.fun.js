function setUTCHours (hour,min,sec,ms) {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. Let h be ToNumber(hour).
    var h = @ToNumber(hour);
    // 3. If min is not specified, then let m be MinFromTime(t); otherwise, let m be ToNumber(min).
    if (!(arguments.length > 1)) {
      var MinutesPerHour = 60;
      var msPerMinute = 60000;
      var MinFromTime_t = @floor(t / msPerMinute) % MinutesPerHour;
      var m = MinFromTime_t;
    }else {
      var m = @ToNumber(min);
    }
    // 4. If sec is not specified, then let s be SecFromTime(t); otherwise, let s be ToNumber(sec).
    if (!(arguments.length > 2)) {
      var SecondsPerMinute= 60;
      var msPerSecond = 1000;
      var SecFromTime_t  = @floor(t / msPerSecond) % SecondsPerMinute;
      var s = SecFromTime_t;
    }else {
      var s = @ToNumber(sec);
    }
    // 5. If ms is not specified, then let milli be msFromTime(t); otherwise, let milli be ToNumber(ms).
    if (!(arguments.length > 3)) {
      var msPerSecond = 1000;
      var msFromTime_t = t % msPerSecond;
      var ms = msFromTime_t;
    }else {
      var milli = @ToNumber(ms);
    }
    // 6. Let newDate be MakeDate(Day(t), MakeTime(h, m, s, milli)).
    var arg1;
    var arg2;
    // Day(t)
    var msPerDay = 86400000;
    var Day_t = @floor(t/msPerDay);
    arg1 = Day_t;
    // MakeTime
    var hour = h;
    var min = m;
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
    var newDate = return_val;
    // 7. Let v be TimeClip(newDate).
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
    // 8. Set the [[PrimitiveValue]] internal property of this Date object to v.
    @PrimitiveValue(this,v);
    // 9. Return v.
    return v;
  }
  