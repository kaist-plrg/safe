function f(x) { return x; }
function g(x) { return f(x); }

var __result1 = g(1);
var __expect1 = 1;
var __result2 = g(2);
var __expect2 = 2;
var __result3 = g(3);
var __expect3 = 3;
var __result4 = g(4);
var __expect4 = 4;
