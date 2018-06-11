function toString() {
    // 1. Let O be the this value.
    var O = this;
    // 2. If Type(O) is not Object, throw a TypeError exception.
    if (typeof O !== 'function' && typeof O !== 'object' || O === null) throw new TypeError();
    // 3. Let name be the result of calling the [[Get]] internal method of O with argument "name".
    var name = O.name;
    // 4. If name is undefined, then let name be "Error"; else let name be ToString(name).
    if (@SameValue(name,undefined)) {
      var name = "Error";
    }else {
      var name = @ToString(name);
    }
    // 5. Let msg be the result of calling the [[Get]] internal method of O with argument "message".
    var msg = O.message;
    // 6. If msg is undefined, then let msg be the empty String; else let msg be ToString(msg).
    if (@SameValue(msg,undefined)) {
      var msg = "";
    }else {
      msg = @ToString(msg);
    }
    // 7. If name is the empty String, return msg.
    if (@SameValue(name,"")) {
      return msg;
    }
    // 8. If msg is the empty String, return name.
    if (@SameValue(msg,"")) {
      return name;
    }
    // 9. Return the result of concatenating name, ":", a single space character, and msg.
    return name + ": " + msg;
  }
  