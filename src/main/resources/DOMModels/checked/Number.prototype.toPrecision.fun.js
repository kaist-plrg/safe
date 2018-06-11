function toPrecision (precision) {
    // 1. Let x be this Number value.
    var x = this;
    // 2. If precision is undefined, return ToString(x).
    if (@SameValue(precision,undefined)) {
      return @ToString(x);
    }
    // 3. Let p be ToInteger(precision).
    var p = @ToInteger(precision);
    // 4. If x is NaN, return the String "NaN".
    if (@SameValue(NaN,x)) {
      return "NaN";
    }
    // 5. Let s be the empty String.
    var s = "";
    // 6. Ifx<0,then
    if (x < 0) {
      // a. Let s be "-".
      var s = "-";
      // b. Letx=–x.
      x = -x;
    }
    // 7. Ifx=+Infinity ,then
    if (@SameValue(x,Infinity)) {
      // a. Return the concatenation of the Strings s and "Infinity".
      return s + "Infinity";
    }
    // 8. If p < 1 or p > 21, throw a RangeError exception.
    if (p < 1 || p > 21) {
      throw new RangeError();
    }
    // 9. Ifx=0,then
    if (x === 0) {
      // a. Let m be the String consisting of p occurrences of the character  ̳0‘.
      var m = "";
      for (var i = 0; i < p; i++) {
        m += "0";
      }
      // b. Lete=0.
      var e = 0
    }
    //10. Else x !=  0,
    else {
      //a. Let e and n be integers such that 10p–1   n < 10p and for which the exact mathematical value of n   10e–p+1 – x is as close to zero as possible. If there are two such sets of e and n, pick the e and n for which n   10e–p+1 is larger.
      // Get e
      if (x_num[0] === "0"){
        var i = 2;
        while (x_num[i] === "0") {
          i += 1
        }
        var e = -i + 1;
      }
      else {
        var i = 0;
        while (x_num[i] !== ".") {
          i += 1;
        }
        e = i - 1;
      }
      // Get n
      var diff = Infinity;
      var pre_diff = Infinity;
      var n = @pow(10,p-1);
      do{
        pre_diff = diff;
        diff = @abs(n * (@pow(10,e-p+1)) - x);
        n += 1;
      }while((diff < pre_diff) && (n < @pow(10,p)))
      n -= 2;
      // b. Let m be the String consisting of the digits of the decimal representation of n (in order, with no leading zeroes).
      var m = @ToString(n);
      // c. Ife < –6 or e>= p,then
      if (e < -6 || e >= p) {
        // i. Let a be the first character of m, and let b be the remaining p–1 characters of m.
        var a = m[0];
        var b = m.slice(1);
        // ii. Let m be the concatenation of the three Strings a, ".", and b.
        m = a + "." + b;
        // iii. If e = 0, then
        if (e === 0) {
          // 1. Let c ="+"and d ="0".
          var c = "+";
          var d = "0";
        }
        //iv. Else e!= 0,
        else {
          // 1. Ife>0, then
          if (e > 0) {
            // a Let c = "+".
            var c = "+";
          }
          // 2. Elsee<0,
          else {
            // a Let c = "-".
            var c = "-";
            // b Let e = –e.
            e = -e;
          }
          // 3. Let d be the String consisting of the digits of the decimal representation of e (in order, with no leading zeroes).
          var d = @ToString(e);
        }
      // v. Let m be the concatenation of the five Strings s, m, "e", c, and d.
      var m = s + m + "e" + c + d;
      }
    }
    // 11. If e = p–1, then return the concatenation of the Strings s and m.
    if (e === p-1) {
      return s + m;
    }
    // 12. If e>= 0,then
    if (e >= 0) {
      // a. Let m be the concatenation of the first e+1 characters of m, the character  ̳.‘, and the remaining p– (e+1) characters of m.
      var m = m.slice(0,e+1) + "." + m.slice(e+1,p);
    }
    // 13. Else e < 0,
    else {
      // a. Let m be the concatenation of the String "0.", –(e+1) occurrences of the character  ̳0‘, and the String m.
      var m = "0.";
      for (var i = 0; i < -(e+1); i++) {
        m += "0";
      }
    }
    // 14. Return the concatenation of the Strings s and m.
    return s + m;
  }
  