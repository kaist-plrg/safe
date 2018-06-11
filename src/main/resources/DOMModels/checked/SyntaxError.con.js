function SyntaxError (message) {
    var obj = {};
    @Prototype(obj,SyntaxError.prototype);
    @Class(obj,"Error");
    @Extensible(obj,true);
    if (!@SameValue(message,undefined)) {
      obj.message = @ToString(message);
    }
    return obj;

  }
  