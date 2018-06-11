function max () {
    var i, n = arguments.length, v = -Infinity;
    for (i = 0; i < n; i++) {
      var cur = @ToNumber(arguments[i])
      if (@SameValue(cur, NaN)) return NaN;
      if (v < cur) v = cur;
      if (@SameValue(v, -0) && @SameValue(cur, +0)) v = +0;
    }
    return v;
  }
  