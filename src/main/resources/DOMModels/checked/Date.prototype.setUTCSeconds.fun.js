function setUTCSeconds (set,ms) {
    // 1. Let t be this time value.
    var t = @PrimitiveValue(this);
    // 2. Let s be ToNumber(sec).
    var s = @ToNumber(sec);
    // 3. If ms is not specified, then let milli be msFromTime(t); otherwise, let milli be ToNumber(ms).
    if (!(arguments.length > 1)) {
      var msPerSecond = 1000;
      var msFromTime_t = t % msPerSecond;
      var milli = msFromTime_t;
    }else {
      var milli = @ToNumber(ms);
    }
    // 4. Let date be MakeDate(Day(t), MakeTime(HourFromTime(t), MinFromTime(t), s, milli)).
    var arg1;
    var arg2;
    var arg3;
    var arg4;
    // HourFromTime(t)
    var HoursPerDay = 24;
    var msPerHour = 3600000;
    var HourFromTime_t = @floor(t / msPerHour) % HoursPerDay;
    arg1 = HourFromTime_t;
    // MinFromTime(t);
    var MinutesPerHour = 60;
    var msPerMinute = 60000;
    var MinFromTime_t = @floor(t / msPerMinute) % MinutesPerHour;
    arg2 = MinFromTime_t;
    // Day(t)
    msPerDay = 86400000;
    var Day_t = @floor(t/msPerDay);
    arg3 = Day_t;
    // MakeTime
    var hour = arg1;
    var min = arg2;
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
    arg4 = return_val;
    // MakeDate(t)
    var day = arg3;
    var time = arg4;
    var return_val;
    var msPerDay = 86400000;
    if (day === Infinity || time === Infinity) {
      return_val = NaN;
    }else {
      return_val = day * msPerDay + time;
    }
    var date = return_val;
    // 5. Let v be TimeClip(date).
    var time = date;
    var return_val;
    if (time === Infinity) {
      return_val = NaN;
    }
    else if(@abs(time) > 8.64e+15) {
      return_val = NaN;
    }else {
      return_val = @ToInteger(time);
    }
    var v = return_val;
    // 6. Set the [[PrimitiveValue]] internal property of this Date object to v.
    @PrimitiveValue(this,v);
    // 7. Return v.
    return v;
  }
  