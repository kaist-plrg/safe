function substr (start, length) {
    // ECMASCript 5 Appendix B.2.3 String.prototype.substr(start, length)
    // 1. Call ToString, giving it the this value as its argument.
    var result1 = @ToString(this);
    // 2. Call ToInteger(start).
    var result2 = @ToInteger(start);
    // 3. If length is undefined, use +; otherwise call ToInteger(length).
    if (@SameValue(length, undefined)) {
      var result3 = Infinity;
    } else {
      var result3 = @ToInteger(length);
    }
    // 4. Compute the number of characters in Result(1).
    var result4 = result1.length;
    // 5. If Result(2) is positive or zero, use Result(2); else use max(Result(4)+Result(2),0).
    //var result5 = if (result2 >= 0) result2 else Math.max(result4+result2,0)
    if (result2 >= 0) {
      var result5 = result2;
    } else {
      var result5 = Math.max(result4+result2,0);
    }
    // 6. Compute min(max(Result(3),0), Result(4)-Result(5)).
    var result6 = Math.min(Math.max(result3, 0), result4-result5)
    // 7. If Result(6) <= 0, return the empty String "".
    if (result6 <= 0) return "";
    // 8. Return a String containing Result(6) consecutive characters from Result(1) beginning with the character at position Result(5).
    else {
      var Str = "";
      for (var i = result5; i < result6; i++) {
        Str = Str + result1[i];
      }
      return Str;
    }
  }
  