function toDateString () {
    // This function returns a String value. The contents of the String are implementation-dependent, but are intended to represent the ―date‖ portion of the Date in the current time zone in a convenient, human-readable form.
    var S = @ToString(this);
    b = 0;
    for (var i = 0; i < S.length; i++) {
      if (S[i] === " ") {
        b += 1;
      }
      if (b === 3) {
        break;
      }
    }
    return S.slice(0,i);
  }
  