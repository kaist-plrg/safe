function ReferenceError (message) {
    var obj = {};
    @Prototype(obj,ReferenceError.prototype);
    @Class(obj,"Error");
    @Extensible(obj,true);
    if (!@SameValue(message,undefined)) {
      obj.message = @ToString(message);
    }
    return obj;
  }
  