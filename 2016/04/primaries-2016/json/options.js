var menuBarInfo = true;
var headers= {
  main: "Democratic voter turnout in the 2016 primary elections",
  sub: "Unofficial data from the state as results may still be coming in.",
  source: "Office of the Secretary of the State",
  byline: "Andrew Ba Tran/TrendCT.org"
};
var menubar = [{
      "var": "turnout",
        "real": "turnout",
        "min": 0,
        "max": 77.5,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "voted",
        "real": "voted",
        "min": 0,
        "max": 16935,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"Town": {
      displayName: "Town",
      munge: function(val) {
          return val;
      }
  },"turnout": {
      displayName: "turnout",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"voted": {
      displayName: "voted",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,255)','rgb(255,255,217)','rgb(237,248,177)','rgb(199,233,180)','rgb(127,205,187)','rgb(65,182,196)','rgb(29,145,192)','rgb(34,94,168)','rgb(37,52,148)','rgb(8,29,88)'];
