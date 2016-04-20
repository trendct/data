var menuBarInfo = true;
var headers= {
  main: "Registered voters by town in Connecticut",
  sub: "Per 100 residents. As of April, 7, 2016.",
  source: "Source: Office of the Secretary of the State",
  byline: "Andrew Ba Tran/TrendCT.org"
};
var menubar = [{
      "var": "Democratic",
        "real": "Democratic",
        "min": 10.77,
        "max": 37.9,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "Republican",
        "real": "Republican",
        "min": 1.43,
        "max": 32.56,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "Unaffiliated",
        "real": "Unaffiliated",
        "min": 9.67,
        "max": 36.8,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"Town": {
      displayName: "Town",
      munge: function(val) {
          return val;
      }
  },"Democratic": {
      displayName: "Democratic",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"Republican": {
      displayName: "Republican",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"Unaffiliated": {
      displayName: "Unaffiliated",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
