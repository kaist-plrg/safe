function indexOf (searchString,position) {
    // 1. Call CheckObjectCoercible passing the this value as its argument.
    if (@SameValue(this,undefined) || @SameValue(this,null)){
      throw new TypeError();
    }
    // 2. Let S be the result of calling ToString, giving it the this value as its argument.
    var S = @ToString(this);
    // 3. Let searchStr be ToString(searchString).
    var searchStr = @ToString(searchString);
    // 4. Let pos be ToInteger(position). (If position is undefined, this step produces the value 0).
    var pos = @ToInteger(position);
    // 5. Let len be the number of characters in S.
    var len = S.length;
    // 6. Let start be min(max(pos, 0), len).
    var start = Math.min(Math.max(pos,0),len);
    // 7. Let searchLen be the number of characters in searchStr.
    var searchLen = searchStr.length;
    //8. Return the smallest possible integer k not smaller than start such that k+ searchLen is not      greater than len,and for all nonnegative integers j less than searchLen, the character at position k+j of S is the same as the character at position j of searchStr; but if there is no such integer k, then return the value -1.
    var k = start;
    while (k+searchLen<=len) {
      var p = 0;
      for (var j=0;j<searchLen;j++) {
        if (S[k+j] !== searchStr[j]) break;
        p+=1;
      }
      if (p === searchLen){
        return k;
      }
      k += 1;
    }
    return -1;
  }
  