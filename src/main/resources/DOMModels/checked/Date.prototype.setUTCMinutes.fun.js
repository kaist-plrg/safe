function setUTCMinutes (min,sec,ms) {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    //2. Let m be ToNumber(min).
    var m = @ToNumber(min);
    // 3. If sec is not specified, then let s be SecFromTime(t); otherwise, let s be ToNumber(sec).
    if (!(arguments.length > 1)) {
      var SecondsPerMinute= 60;
      var msPerSecond = 1000;
      var SecFromTime_t  = @floor(t / msPerSecond) % SecondsPerMinute;
      var sec = SecFromTime_t;
    }else {
      var s = @ToNumber(sec);
    }
    // 4. If ms is not specified, then let milli be msFromTime(t); otherwise, let milli be ToNumber(ms).
    if (!(arguments.length > 2)) {
      var msPerSecond = 1000;
      var msFromTime_t = t % msPerSecond;
      var milli = msFromTime_t;
    }else {
      var milli = @ToNumber(ms);
    }
    // 5. Let date be MakeDate(Day(t), MakeTime(HourFromTime(t), m, s, milli)).
    var arg1;
    var arg2;
    var arg3;
    // HourFromTime(t)
    var HoursPerDay = 24;
    var msPerHour = 3600000;
    var HourFromTime_t = @floor(t / msPerHour) % HoursPerDay;
    arg1 = HourFromTime_t;
    // Day(t)
    var msPerDay = 86400000;
    var Day_t = @floor(t/msPerDay);
    arg2 = Day_t;
    // MakeTime
    var hour = arg1;
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
      var mlli = @ToInteger(ms);
      var t = h * msPerHour + m * msPerMinute + s * msPerSecond + milli;
      return_val = t;
    }
    arg3 = return_val;
    // MakeDate
    var day = arg2;
    var time = arg3;
    var return_val;
    var msPerDay = 86400000;
    if (day === Infinity || time === Infinity) {
      return_val = NaN;
    }else {
    return_val = day * msPerDay + time;
    }
    var date = return_val;
    // 6. Let v be TimeClip(date).
    // TimeClip
    var time = date;
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
    // 7. Set the [[PrimitiveValue]] internal property of this Date object to v.
    @PrimitiveValue(this,v);
    //8. Return v.
    return v;
  }
  