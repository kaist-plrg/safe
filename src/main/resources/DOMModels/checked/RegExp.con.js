function RegExp () {
    var obj = {};
    @Class(obj,"RegExp");
    @Prototype(obj,RegExp.prototype);
    obj.source = @StrTop;
    obj.global = @BoolTop;
    obj.ignoreCase = @BoolTop;
    obj.multiline = @BoolTop;
    obj.lastIndex = @NumTop;
    return obj;
  }
  