var menuBarInfo = true;
var headers= {
  main: "Percent of Republican primary votes by candidate",
  sub: "Out of those who voted.",
  source: "Office of the Secretary of the State",
  byline: "Andrew Ba Tran/TrendCT.org"
};
var menubar = [{
      "var": "Romney",
        "real": "Romney",
        "min": 15.49,
        "max": 50.44,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "McCain",
        "real": "McCain",
        "min": 34.96,
        "max": 65.37,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "Huckabee",
        "real": "Huckabee",
        "min": 1.71,
        "max": 21.5,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"Town": {
      displayName: "Town",
      munge: function(val) {
          return val;
      }
  },"Romney": {
      displayName: "Romney",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"McCain": {
      displayName: "McCain",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"Huckabee": {
      displayName: "Huckabee",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
