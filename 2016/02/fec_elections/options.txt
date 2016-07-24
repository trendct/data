var menuBarInfo = true;
var headers= {
  main: "Donations to Presidential candidates in 2015",
  sub: "",
  source: "Federal Election Commission",
  byline: "Andrew Ba Tran/TrendCT.org"
};
var menubar = [{
      "var": "Donations",
        "real": "Donations",
        "min": 1,
        "max": 1004,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "Donation sum",
        "real": "Donation sum",
        "min": 12.5,
        "max": 1320934.37,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"Town": {
      displayName: "Town",
      munge: function(val) {
          return val;
      }
  },"Donations": {
      displayName: "Donations",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"Donation sum": {
      displayName: "Donation sum",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
