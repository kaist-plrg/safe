  function testcase() 
  {
    var obj = {
      
    };
    var fun = (function () 
    {
      return "ownDataProperty";
    });
    Object.defineProperty(obj, "property", {
      get : fun,
      configurable : true
    });
    var desc = Object.getOwnPropertyDescriptor(obj, "property");
    var accessed = false;
    for(var prop in desc)
    {
      if (prop === "get")
      {
        accessed = true;
      }
    }
    return accessed;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  