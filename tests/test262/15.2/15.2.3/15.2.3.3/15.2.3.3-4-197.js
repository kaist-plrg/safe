  function testcase() 
  {
    var desc = Object.getOwnPropertyDescriptor(Number, "MIN_VALUE");
    if (desc.writable === false && desc.enumerable === false && desc.configurable === false && desc.hasOwnProperty('get') === false && desc.hasOwnProperty('set') === false)
    {
      return true;
    }
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  