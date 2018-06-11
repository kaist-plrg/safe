function toLocaleTimeString () {
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
    return S.slice(i);
  }
  