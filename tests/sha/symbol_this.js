function f() { return this; }

var __result1 = {f: f, p: 1}.f().p;
var __expect1 = 1;
var __result2 = {f: f, p: 2}.f().p;
var __expect2 = 2;
var __result3 = {f: f, p: 3}.f().p;
var __expect3 = 3;
var __result4 = {f: f, p: 4}.f().p;
var __expect4 = 4;
