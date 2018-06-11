function String (value) {
    // Returns a String value (not a String object) computed by ToString(value). If value is not supplied, the empty String "" is returned.
    if(arguments.length === 0){
      return "";
    }else{
      return @ToString(value);
    }
  }
  