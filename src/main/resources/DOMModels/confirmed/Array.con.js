function Array (len) {
    var arr = [];
    if (arguments.length === 1) {
      if (typeof len === 'number') {
        if (@SameValue(@ToUint32(len), len)) arr.length = len;
        else throw new RangeError();
      } else {
        arr.length = 1;
        arr[0] = len;
      }
    } else {
      for (var i = 0; i < arguments.length; i++) {
        arr[i] = arguments[i];
      }
      arr.length = arguments.length;
    }
    return arr;
  }
  