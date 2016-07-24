var menuBarInfo = true;
var headers= {
  main: "Test map for R generator",
  sub: "This is a subhead",
  source: "airbnb.com",
  byline: "TrendCT.org"
};
var menubar = [{
      "var": "airbnbs",
        "real": "airbnbs",
        "min": 1,
        "max": 247,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "avg.per.bed",
        "real": "avg.per.bed",
        "min": 22,
        "max": 482.75,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "median.per.bed",
        "real": "median.per.bed",
        "min": 22,
        "max": 200,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "max.price",
        "real": "max.price",
        "min": 40,
        "max": 5000,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"town": {
      displayName: "town",
      munge: function(val) {
          return val;
      }
  },"airbnbs": {
      displayName: "airbnbs",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"avg.per.bed": {
      displayName: "avg.per.bed",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"median.per.bed": {
      displayName: "median.per.bed",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"max.price": {
      displayName: "max.price",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
