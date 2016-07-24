$(function() {

    var chart = new TrendChart({
        container_div_id: "chart_container",
        sourceline: "Department of Public Health",
        title: "Sources of lead hazards in 2013",
        explainer: "Officials investigated 137 homes of lead-poisoned children.",
        byline: "Andrew Ba Tran / TrendCT.org",
        bar_spacing: 1,
        bar_height: 40,
        x_padding: 20,
        //override horizontal alignment relative to svg left:0
        //bar_left_padding: 60,
        data: {
            // override max scale
            max_val: 100,
            value_suffix: "%",
            //value_prefix: "",
            //value_prefix: "",
            bars: [{
                "label": "Environmental lead hazards",
                "value": 86.1
            }, {
                "label": "Multiple-unit dwellings",
                "value": 73.7
            }, {
                "label": "Paint hazards",
                "value": 81
            }, {
                "label": "Dust hazards",
                "value": 50.4
            }, {
                "label": "Soil hazards",
                "value": 32.8
            }, {
                "label": "Water hazard",
                "value": 0
            }]
        },
    });

});
