function toString (radix) {
    // The toString function is not generic; it throws a TypeError exception if its this value is not a Number or a Number object.
    if (!@SameValue(typeof this,'number') && !@SameValue(@Class(this), 'Number')){
      throw new TypeError();
    }
    // If radix not present or is undefined the Number 10 is used as the value of radix.
    if (arguments.length === 0 || @SameValue(radix,undefined)){
      var radix = 10;
    }
    else {
      radix = @ToInteger(radix);
    }
    // If ToInteger(radix) is the Number 10 then this Number value is given as an argument to the ToString abstract operation; the resulting String value is returned.
    if (radix === 10) {
      return @ToString(this);
    }
    // If ToInteger(radix) is not an integer between 2 and 36 inclusive throw a RangeError exception.
    else if (radix < 2 || radix > 36){
      throw new RangeError();
    }
    // If ToInteger(radix) is an integer from 2 to 36, but not 10, the result is a String representation of this Number value using the specified radix. Letters a-z are used for digits with values 10 through 35. The precise algorithm is implementation-dependent if the radix is not 10, however the algorithm should be a generalisation of that specified in 9.8.1.
    else if (radix >= 2 && radix <= 36) {
      var S = "";
      var n = this;
      var alpha = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"];
      while (n > 0){
        if(n%radix >= 10){
          S = alpha[n%radix - 10] + S;
        }else {
          S = @ToString(n%radix) + S;
        }
        n = @floor(n / radix);
      }
      return S;
    }
  }
  