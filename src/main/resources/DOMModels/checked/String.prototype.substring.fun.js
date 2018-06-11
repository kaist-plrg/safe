function substring (start,end ) {
    // 1. Call CheckObjectCoercible passing the this value as its argument.
    if(@SameValue(this,undefined) || @SameValue(this,null)) {
      throw new TypeError();
    }
    // 2. Let S be the result of calling ToString, giving it the this value as its argument.
    var S = @ToString(this);
    // 3. Let len be the number of characters in S.
    var len = S.length;
    // 4. Let intStart be ToInteger(start).
    var intStart = @ToInteger(start);
    // 5. If end is undefined, let intEnd be len; else let intEnd be ToInteger(end).
    if (@SameValue(end,undefined)){
      var intEnd = len;
    }else {
      var intEnd = @ToInteger(end);
    }
    // 6. Let finalStart be min(max(intStart, 0), len).
    var finalStart = Math.min(Math.max(intStart,0),len);
    // 7. Let finalEnd be min(max(intEnd, 0), len).
    var finalEnd = Math.min(Math.max(intEnd,0),len);
    // 8. Let from be min(finalStart, finalEnd).
    var from = Math.min(finalStart,finalEnd);
    // 9. Let to be max(finalStart, finalEnd).
    var to = Math.max(finalStart,finalEnd);
    // 10. Return a String whose length is to - from, containing characters from S, namely the characters with indices from through to -1, in ascending order.
    var Str = "";
    for (var i = 0; i<(to-from);i++){
      Str = Str + S[from+i];
    }
    return Str;
  }
  