function f() { return {}; }

var obj1 = f();
obj1.p = 1;
var __result1 = obj1.p;
var __expect1 = 1;

var obj2 = f();
obj2.p = 2;
var __result2 = obj2.p;
var __expect2 = 2;

var obj3 = f();
obj3.p = 3;
var __result3 = obj3.p;
var __expect3 = 3;

var obj4 = f();
obj4.p = 4;
var __result4 = obj4.p;
var __expect4 = 4;
