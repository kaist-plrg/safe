function slice (start,end) {
    // 1. Call CheckObjectCoercible passing the this value as its argument.
    if (@SameValue(undefined,this) || @SameValue(null,this)){
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
    // 6. If intStart is negative, let from be max(len + intStart,0); else let from be min(intStart, len).
    if (intStart < 0) {
      var from = Math.max(len+intStart,0);
    }else {
      var from = Math.min(intStart,len);
    }
    // 7. If intEnd is negative, let to be max(len + intEnd,0); else let to be min(intEnd, len).
    if (intEnd < 0){
      var to = Math.max(len + intEnd,0);
    }else {
      var to = Math.min(intEnd,len);
    }
    // 8. Let span be max(to – from,0).
    var span = Math.max(to-from,0);
    // 9. Return a String containing span consecutive characters from S beginning with the character at   position from.
    var Str = "";
    for (var i = 0;i<span;i++) {
      Str = Str + S[from+i];
    }
    return Str;
  }
  