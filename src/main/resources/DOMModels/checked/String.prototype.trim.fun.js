function trim () {
    // 1. Call CheckObjectCoercible passing the this value as its argument.
    if (@SameValue(undefined,this) || @SameValue(null,this)){
      throw new TypeError();
    }
    // 2. Let S be the result of calling ToString, giving it the this value as its argument.
    var S = @ToString(this);
    // 3. Let T be a String value that is a copy of S with both leading and trailing white space removed. The definition of white space is the union of WhiteSpace and LineTerminator.
    var f = 0;
    while (S[f] === " "){
      f += 1;
    }
    var t = S.length - 1;
    while (S[t] === " "){
      t -= 1;
    }
    t +=1;
    var T = S.substring(f,t);
    // 4. Return T.
    return T;
  }
  