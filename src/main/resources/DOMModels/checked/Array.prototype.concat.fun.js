function concat () {
    // 1. Let O be the result of calling ToObject passing the this value as the argument.
    var O = @ToObject(this);
    // 2. Let A be a new array created as if by the expression new Array() where Array is the standard built-in constructor with that name.
    var A = new Array();
    //3. Let n be 0.
    var n = 0;
    // for O
    var E = O;
    if(typeof E === 'object' && E !== null && @Class(E) === "Array"){
      var k = 0;
      var len = E.length;
      while(k<len){
        var P = @ToString(k);
        var exists = (P in E);
        if (exists) {
          var subElement = E[P];
          @DefineOwnProperty(A,@ToString(n),{value:subElement, writable:true, enumerable:true,       configurable:true});
        }
        n += 1;
        k += 1;
      }
    }
    else {
      Object.defineProperty(A,@ToString(n),{value:E,writable:true,enumerable:true,configurable:      true});
      n += 1;
    }
    // 4. Let items be an internal List whose first element is O and whose subsequent elements are, in left to right order, the arguments that were passed to this function invocation.
    var items = arguments;
    // 5. Repeat, while items is not empty
    for (var i = 0; i < items.length; i++) {
      // a. Remove the first element from items and let E be the value of the element.
      var E = items[i];
      // b. If the value of the [[Class]] internal property of E is "Array", then
      if(typeof E === 'object' && E !== null && @Class(E) === "Array"){
        // i. Let k be 0.
        var k = 0;
        // ii. Let len be the result of calling the [[Get]] internal method of E with argument "length".
        var len = E.length;
        // iii. Repeat, while k < len
        while(k<len){
          // 1. Let P be ToString(k).
          var P = @ToString(k);
          // 2. Let exists be the result of calling the [[HasProperty]] internal method of E with P.
          var exists = (P in E);
          // 3. If exists is true, then
          if (exists) {
            // a Let subElement be the result of calling the [[Get]] internal method of E with         argument P.
            var subElement = E[P];
            // b Call the [[DefineOwnProperty]] internal method of A with arguments ToString(n),       Property Descriptor {[[Value]]: subElement, [[Writable]]: true, [[Enumerable]]: true,                  [[Configurable]]: true}, and false.
            @DefineOwnProperty(A,@ToString(n),{value:subElement, writable:true, enumerable:true,       configurable:true});
          }
          // 4. Increase n by 1.
          n += 1;
          // 5. Increase k by 1.
          k += 1;
        }
      }
      // c. Else, E is not an Array
      else {
        // i. Call the [[DefineOwnProperty]] internal method of A with arguments ToString(n), Property Descriptor {[[Value]]: E, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false.
        Object.defineProperty(A,@ToString(n),{value:E,writable:true,enumerable:true,configurable:      true});
        // ii. Increase n by 1.
        n += 1;
      }
    }
    A.length = n;
    // 6. Return A.
    return A;
  }
  