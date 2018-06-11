function toFixed (fractionDigits){
    // 1. Let f be ToInteger(fractionDigits). (If fractionDigits is undefined, this step produces the value 0).
    var f = @ToInteger(fractionDigits);
    // 2. If f < 0 or f > 20, throw a RangeError exception.
    if (f < 0 || f > 20) {
      throw new RangeError();
    }
    // 3. Let x be this Number value.
    var x = this;
    // 4. If x is NaN, return the String "NaN".
    if (@SameValue(x,NaN)) {
      return "NaN";
    }
    // 5. Let s be the empty String.
    var s = "";
    // 6. Ifx<0,then
    if ( x < 0) {
      // a. Let s be "-".
      var s = "-";
      // b. Letx=–x.
      x = -x;
    }
    // 7. If x >= 10^21, then
    if (x >= 10e+21) {
      // a. Let m = ToString(x).
      var m = @ToString(x);
    }
    // 8. Else, x < 10^21
    else {
      // a. Let n be an integer for which the exact mathematical value of n/10^f – x is as close to zero as possible. If there are two such n, pick the larger n.
      var diff = Infinity;
      var pre_diff = Infinity;
      var n = 0;
      do{
        pre_diff = diff;
        diff = @abs(n / (@pow(10,f)) - x);
        n += 1;
      }while(diff < pre_diff);
      n -= 2;
      // b. If n = 0, let m be the String "0". Otherwise, let m be the String consisting of the digits of the decimal representation of n (in order, with no leading zeroes).
      if (n === 0) {
        var m = "0";
      }else {
        var m = @ToString(n);
      }
      // c. If f!=0,then
      if (f != 0) {
        // i. Let k be the number of characters in m.
        var k = m.length;
        // ii. If k ≤ f, then
        if ( k <= f) {
          // 1. Let z be the String consisting of f+1–k occurrences of the character  ̳0‘.
          var z = "";
          for ( var i = 0; i < f + 1 - k; i++) {
            z = z + "0";
          }
          // 2. Let m be the concatenation of Strings z and m.
          var m = z + m;
          // 3. Letk=f+1.
          var k = f + 1;
        }
        // iii. Let a be the first k–f characters of m, and let b be the remaining f characters of m.
        var a = m.slice(0,k-f);
        var b = m.slice(k-f);
        // iv. Let m be the concatenation of the three Strings a, ".", and b.
        var m = a + "." + b;
      }
    }
    // 9. Return the concatenation of the Strings s and m.
    return s + m;
  }
  