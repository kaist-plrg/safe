function EvalError (message) {
    var obj = {};
    @Prototype(obj,EvalError.prototype);
    @Class(obj,"Error");
    @Extensible(obj,true);
    if (!@SameValue(message,undefined)) {
      obj.message = @ToString(message);
    }
    return obj;
  }
  