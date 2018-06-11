function isNaN (number) {
    // 1. If ToNumber(number) is NaN, return true.
    // 2. Otherwise, return false.
    return @SameValue(@ToNumber(number), NaN);
  }
  