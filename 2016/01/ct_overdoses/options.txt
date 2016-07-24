var menuBarInfo = true;
var headers= {
  main: "Accidental drug overdoes by town",
  sub: "Between 2012 and Sept. 2015",
  source: "State Medical Examiner's Office",
  byline: "TrendCT.org"
};
var menubar = [{
      "var": "Population",
        "real": "Population",
        "min": 1041,
        "max": 145587,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "Heroin.overdoses",
        "real": "Heroin.overdoses",
        "min": NA,
        "max": NA,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "opioid.overdoses",
        "real": "opioid.overdoses",
        "min": NA,
        "max": NA,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"Town": {
      displayName: "Town",
      munge: function(val) {
          return val;
      }
  },"Population": {
      displayName: "Population",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"Heroin.overdoses": {
      displayName: "Heroin.overdoses",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"opioid.overdoses": {
      displayName: "opioid.overdoses",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
