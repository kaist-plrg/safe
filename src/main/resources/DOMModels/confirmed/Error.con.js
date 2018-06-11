function Error (message) {
    var obj ={};
    @Prototype(obj,Error.prototype);
    @Class(obj,"Error");
    @Extensible(obj,true);
    if (!@SameValue(message,undefined)) {
      obj.message = @ToString(message);
    }
    return obj;
  }
  