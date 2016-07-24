var menuBarInfo = true;
var headers= {
  main: "Subsidies to businesses in Connecticut by town",
  sub: "Since 1999.",
  source: "subsidytracker.goodjobsfirst.org",
  byline: "TrendCT.org"
};
var menubar = [{
      "var": "subsidies",
        "real": "subsidies",
        "min": 8500,
        "max": 997697101,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "count",
        "real": "count",
        "min": 1,
        "max": 144,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"town": {
      displayName: "town",
      munge: function(val) {
          return val;
      }
  },"subsidies": {
      displayName: "subsidies",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"count": {
      displayName: "count",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
