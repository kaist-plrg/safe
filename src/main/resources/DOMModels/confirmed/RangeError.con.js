function RangeError (message) {
    var obj = {};
    @Prototype(obj,RangeError.prototype);
    @Class(obj,"Error");
    @Extensible(obj,true);
    if (!@SameValue(message,undefined)) {
      obj.message = @ToString(message);
    }
    return obj;
  }
  