  var x = new Array("1");
  {
    var __result1 = x.length !== 1;
    var __expect1 = false;
  }
  {
    var __result2 = x[0] !== "1";
    var __expect2 = false;
  }
  var obj = new String("0");
  var x = new Array(obj);
  {
    var __result3 = x.length !== 1;
    var __expect3 = false;
  }
  {
    var __result4 = x[0] !== obj;
    var __expect4 = false;
  }
  