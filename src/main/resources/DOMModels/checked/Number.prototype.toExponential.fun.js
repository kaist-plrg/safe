function toExponential (fractionDigits) {
    // 1. Let x be this Number value.
    var x = this;
    // 2. Let f be ToInteger(fractionDigits).
    var f = @ToInteger(fractionDigits);
    // 3. If x is NaN, return the String "NaN".
    if (@SameValue(x,NaN)) {
      return "NaN"
    }
    // 4. Let s be the empty String.
    var s = "";
    // 5. Ifx<0,then
    if (x < 0) {
      // a. Let s be "-".
      var s = "-";
      // b. Letx=–x.
      x = -x;
    }
    // 6. Ifx=Infinity,then
    if (@SameValue(x,Infinity)) {
      // a. Return the concatenation of the Strings s and "Infinity".
      return s + "Infinity";
    }
    // 7. If fractionDigits is not undefined and (f < 0 or f > 20), throw a RangeError exception.
    if ((!@SameValue(fractionDigits,undefined)) && (f < 0 || f > 20)) {
      throw new RangeError();
    }
    // 8. Ifx=0,then
    if (x === 0) {
      // a. Letf=0.
      var f = 0;
      // b. Let m be the String consisting of f+1 occurrences of the character  ̳0‘.
      m = "";
      for (var i = 0; i < f+1; i++) {
        m += "0";
      }
      // c. Lete=0.
      var e = 0;
    }
    // 9. Else,x!=0
    else {
      // a. If fractionDigits is not undefined, then
      if (!@SameValue(fractionDigits,undefined)) {
        // i. Let e and n be integers such that 10f   n < 10f+1 and for which the exact mathematical value of n 10e–f –x is as close to zero as possible. If there are two such sets of e and n, pick the e and n for which n   10e–f is larger.
        x_num = @ToString(x);
        // Get e
        if (x_num[0] === "0"){
          var i = 2;
          while (x_num[i] === "0") {
            i += 1;
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
        var n = @pow(10,f);
        do{
          pre_diff = diff;
          diff = @abs(n * (@pow(10,e-f)) - x);
          n += 1;
        }while((diff < pre_diff) && (n < @pow(10,f+1)));
        n -= 2;
      }
      // b. Else, fractionDigits is undefined
      else {
        // i. Lete,n,andfbeintegerssuchthatf 0,10f  n<10f+1,thenumbervalueforn 10e–f is x,and f is as small as possible. Note that the decimal representation of n has f+1 digits, n is not divisible by 10, and the least significant digit of n is not necessarily uniquely determined by these criteria.
        x_num = @ToString(x);
        // Get f
        if (x_num[0] === 0) {
          var i = 2;
          while (x_num[i] === "0") {
            i += 1;
          }
          f = x_num.length - i - 1;
        }else {
          f = x_num.length - 2;
        }
        // Get e
        if (x_num[0] === "0"){
          var i = 2;
          while (x_num[i] === "0") {
            i += 1;
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
        var n = @pow(10,f);
        do{
          pre_diff = diff;
          diff = @abs(n * (@pow(10,e-f)) - x);
          n += 1;
        }while((diff < pre_diff) && (n < @pow(10,f+1)));
        n -= 2;
      }
      // c. Let m be the String consisting of the digits of the decimal representation of n (in order, with no leading zeroes).
      var m = @ToString(n);
    }
    // 10. If f!=0,then
    if (f != 0) {
      // a. Let a be the first character of m, and let b be the remaining f characters of m.
      var a = m.slice(0,1);
      var b = m.slice(1);
      // b. Let m be the concatenation of the three Strings a, ".", and b.
      m = a + "." + b;
    }
    // 11. Ife=0,then
    if (e === 0) {
      // a. Letc="+".
      var c = "+";
      // b. Letd="0".
      var d = 0;
    }
    // 12. Else
    else {
      // a. Ife>0,thenletc="+".
      if (e > 0) {
        var c = "+";
      }
      // b. Else,e≤0
      else {
        // i. Let c = "-".
        c = "-";
        // ii. Let e = –e.
        e = -e;
      }
      // c. Let d be the String consisting of the digits of the decimal representation of e (in order, with no leading zeroes).
      d = @ToString(e);
    }
    // 13. Let m be the concatenation of the four Strings m, "e", c, and d.
    var m = m + "e" + c + d;
    // 14. Return the concatenation of the Strings s and m.
    return m;
  }
  