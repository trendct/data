var menuBarInfo = true;
var headers= {
  main: "Test map for R generator2",
  sub: "This is a subhead",
  source: "something",
  byline: "TrendCT.org"
};
var menubar = [{
      "var": "Count",
        "real": "Count",
        "min": 1,
        "max": 14,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "Population",
        "real": "Population",
        "min": 25,
        "max": 4233,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"real.town.name": {
      displayName: "real.town.name",
      munge: function(val) {
          return val;
      }
  },"Count": {
      displayName: "Count",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"Population": {
      displayName: "Population",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
