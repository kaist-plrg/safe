function toUpperCase () {
    // This function behaves in exactly the same way as String.prototype.toLowerCase, except that characters are mapped to their uppercase equivalents as specified in the Unicode Character Database.
    if(@SameValue(this,undefined) || @SameValue(this,null)) {
      throw new TypeError();
    }
    var S = @ToString(this);
    alpha = {"a":"A","b":"B","c":"C","d":"D","e":"E","f":"F","g":"G","h":"H","i":"I","j":"J","k":"K","l":"L","m":"M","n":"N","o":"O","p":"P","q":"Q","r":"R","s":"S","t":"T","u":"U","v":"V","w":"W","x":"X","y":"Y","z":"Z"}
    L = "";
    for (var i = 0; i < S.length; i++){
      if (S[i] in alpha) {
        L += alpha[S[i]];
      }else {
        L += S[i];
      }
    }
    return L;
  }
  