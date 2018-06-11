function sort (comparefn) {
    // modeled using bubble sort (http://en.wikipedia.org/wiki/Bubble_sort)
    var O = @ToObject(this);
    var len = O.length;
    if (@SameValue(comparefn,undefined)) {
      comparefn = function (left,right) {
        if (left < right) {
          return -1;
        }else if (left == right) {
          return 0;
        }else if (left > right) {
          return 1;
        }
      }
    }
    do {
      var newn = 0;
      for (var i = 1; i< n; i++) {
        var result = compare(a[i-1],a[i]);
        // a[i-1] > a[i]
        if (result === 1) {
          var temp = a[-1];
          a[i-1] = a[i];
          a[i] = temp;
          newn = i;
        }
      }
      n = newn;
    }while (n != 0);
    return O;
  }
  