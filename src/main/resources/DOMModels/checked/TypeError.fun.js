function TypeError (message) {
    var obj = {};
    @Prototype(obj,TypeError.prototype);
    @Class(obj,"Error");
    @Extensible(obj,true);
    if (!@SameValue(message,undefined)) {
      obj.message = @ToString(message);
    }
    return obj;
  }
  