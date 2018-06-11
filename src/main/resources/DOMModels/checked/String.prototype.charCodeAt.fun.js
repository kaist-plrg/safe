function charCodeAt (pos) {
    // 1. Call CheckObjectCoercible passing the this value as its argument.
    if((@SameValue(undefined,this)) || (@SameValue(null,this))){
      throw new TypeError();
    }
    // 2. Let S be the result of calling ToString, giving it the this value as its argument.
    var S = @ToString(this);
    // 3. Let position be ToInteger(pos).
    var position = @ToInteger(pos);
    // 4. Let size be the number of characters in S.
    var size = S.length;
    // 5. If position < 0 or position ≥ size, return NaN.
    if((position<0) || (position>=size)){
      return NaN;
    }
    // 6. Return a value of Number type, whose value is the code unit value of the character at position position in the String S, where the first (leftmost) character in S is considered to be at position 0, the next one at position 1, and so on.
    // TODO implement @ToUint16
    // Need char -> Unicode-16
    // return @ToUint16(S[position]);
    return @ToNumber(S[position]);
  }
  