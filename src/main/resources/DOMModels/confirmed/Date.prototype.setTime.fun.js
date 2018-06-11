function setTime (time) {
    // 1. Let v be TimeClip(ToNumber(time)).
    var time = @ToNumber(time);
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
    //  2. Set the [[PrimitiveValue]] internal property of this Date object to v.
    @PrimitiveValue(this,v);
    // 3. Return v.
    return v;
  }
  