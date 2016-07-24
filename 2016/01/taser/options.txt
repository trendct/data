var menuBarInfo = true;
var headers= {
  main: "Reported stun gun incidents by town department",
  sub: "Data from 2015. Per capita is per 10,000 residents. Some towns do not appear on this map because they are under the jurisdiction of the State Police. These police departments did not submit a report: Middlebury, Stratford, Thomaston, Weston, and Winchester.",
  source: "Office of Policy and Magagement",
  byline: "Andrew Ba Tran/TrendCT.org"
};
var menubar = [{
      "var": "Reported.Incidents",
        "real": "Reported.Incidents",
        "min": 0,
        "max": 51,
        "munge": function(val) {
            return val;
        }
      },{
      "var": "incidents.per.capita",
        "real": "incidents.per.capita",
        "min": 0,
        "max": 7.97,
        "munge": function(val) {
            return val;
        }
      }]
                 var hoverName = {"real.town.name": {
      displayName: "real.town.name",
      munge: function(val) {
          return val;
      }
  },"Reported.Incidents": {
      displayName: "Reported.Incidents",
      munge: function(val) {
        return checkNull(comma(val));
      }
  },"incidents.per.capita": {
      displayName: "incidents.per.capita",
      munge: function(val) {
        return checkNull(comma(val));
      }
  }
}
var colors = ['rgb(255,255,240)','rgb(255,255,204)','rgb(255,237,160)','rgb(254,217,118)','rgb(254,178,76)','rgb(253,141,60)','rgb(252,78,42)','rgb(227,26,28)','rgb(189,0,38)','rgb(128,0,38)'];
