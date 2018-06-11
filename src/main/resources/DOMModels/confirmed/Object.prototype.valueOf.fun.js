function valueOf () {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // XXX: We do not support host object.
    // 2. If O is the result of calling the Object constructor with a host object (15.2.2.1), then
    //   a. Return either O or another value such as the host object originally passed to the constructor.
    //      The specific result that is returned is implementation-defined.
    // 3. Return O
    return O;
  }
  