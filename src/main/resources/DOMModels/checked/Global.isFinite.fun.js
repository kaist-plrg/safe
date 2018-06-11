function isFinite (number) {
    // 1. If ToNumber(number) is NaN, +Inf or -Inf, return false.
    // 2. Otherwise, return true.
    var num = @ToNumber(number);
    return !@SameValue(num, NaN)
      && !@SameValue(num, Infinity)
      && !@SameValue(num, -Infinity);
  }
  