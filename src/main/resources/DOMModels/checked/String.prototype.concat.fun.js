function concat () {
    // 1. Call CheckObjectCoercible passing the this value as its argument.
    if (@SameValue(this,undefined) || @SameValue(this,null)){
      throw new TypeError();
    }
    // 2. Let S be the result of calling ToString, giving it the this value as its argument.
    var S = @ToString(this);
    // 3. Let args be an internal list that is a copy of the argument list passed to this function.
    var args = arguments;
    // 4. Let R be S.
    var R = S;
    // 5. Repeat, while args is not empty
    for (var i = 0; i < args.length; i++) {
      // a. Remove the first element from args and let next be the value of that element.
      var next = args[i];
      // b. Let R be the String value consisting of the characters in the previous value of R followed by the characters of ToString(next).
      var R = R + @ToString(next);
    }
    // 6. Return R.
    return R;
  }
  