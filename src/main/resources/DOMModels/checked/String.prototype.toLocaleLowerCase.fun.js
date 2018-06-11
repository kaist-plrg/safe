function toLocaleLowerCase () {
    // 1. Call CheckObjectCoercible passing the this value as its argument.
    if(@SameValue(this,undefined) || @SameValue(this,null)) {
      throw new TypeError();
    }
    //2. Let S be the result of calling ToString, giving it the this value as its argument.
    var S = @ToString(this);
    //3. Let L be a String where each character of L is either the Unicode lowercase equivalent of the corresponding character of S or the actual corresponding character of S if no Unicode lowercase equivalent exists.
    alpha = {"A":"a","B":"b","C":"c","D":"d","E":"e","F":"f","G":"g","H":"h","I":"i","J":"j","K":"k","L":"l","M":"m","N":"n","O":"o","P":"p","Q":"q","R":"r","S":"s","T":"t","U":"u","V":"v","W":"w","X":"x","Y":"y","Z":"z"}
    L = "";
    for (var i = 0; i < S.length; i++){
      if (S[i] in alpha) {
        L += alpha[S[i]];
      }else {
        L += S[i];
      }
    }
    //4. Return L.
    return L;
  }
  