  x = [];
  var object = {
    valueOf : (function () 
    {
      return 1;
    })
  };
  x[object] = 0;
  {
    var __result1 = x["[object Object]"] !== 0;
    var __expect1 = false;
  }
  x = [];
  var object = {
    valueOf : (function () 
    {
      return 1;
    }),
    toString : (function () 
    {
      return 0;
    })
  };
  x[object] = 0;
  {
    var __result2 = x[0] !== 0;
    var __expect2 = false;
  }
  x = [];
  var object = {
    valueOf : (function () 
    {
      return 1;
    }),
    toString : (function () 
    {
      return {
        
      };
    })
  };
  x[object] = 0;
  {
    var __result3 = x[1] !== 0;
    var __expect3 = false;
  }
  try
{    x = [];
    var object = {
      valueOf : (function () 
      {
        throw "error";
      }),
      toString : (function () 
      {
        return 1;
      })
    };
    x[object] = 0;
    {
      var __result4 = x[1] !== 0;
      var __expect4 = false;
    }}
  catch (e)
{    if (e === "error")
    {
      $ERROR('#4.2: x = []; var object = {valueOf: function() {throw "error"}, toString: function() {return 1}}; x[object] = 0; x[1] === 1. Actual: ' + ("error"));
    }
    else
    {
      $ERROR('#4.3: x = []; var object = {valueOf: function() {throw "error"}, toString: function() {return 1}}; x[object] = 0; x[1] === 1. Actual: ' + (e));
    }}

  x = [];
  var object = {
    toString : (function () 
    {
      return 1;
    })
  };
  x[object] = 0;
  {
    var __result5 = x[1] !== 0;
    var __expect5 = false;
  }
  x = [];
  var object = {
    valueOf : (function () 
    {
      return {
        
      };
    }),
    toString : (function () 
    {
      return 1;
    })
  };
  x[object] = 0;
  {
    var __result6 = x[1] !== 0;
    var __expect6 = false;
  }
  try
{    x = [];
    var object = {
      valueOf : (function () 
      {
        return 1;
      }),
      toString : (function () 
      {
        throw "error";
      })
    };
    x[object];
}
  catch (e)
{    {
      var __result7 = e !== "error";
      var __expect7 = false;
    }}

  try
{    x = [];
    var object = {
      valueOf : (function () 
      {
        return {
          
        };
      }),
      toString : (function () 
      {
        return {
          
        };
      })
    };
    x[object];
}
  catch (e)
{    {
      var __result8 = (e instanceof TypeError) !== true;
      var __expect8 = false;
    }}

  
