$(function() {

    var chart = new TrendChart({
        container_div_id: "chart_container",
        sourceline: "Federal Communications Commission, politicaladsleuth.com",
        title: "2014 FCC ad filings in Connecticut",
        explainer: "The 10-most filings at local tv stations.",
        byline: "Andrew Ba Tran  / TrendCT.org",
        bar_spacing: 1,
        bar_height: 40,
        x_padding: 20,
        //override horizontal alignment relative to svg left:0
        //bar_left_padding: 60,
        data: {
            // override max scale
            max_val: 800,
            value_suffix: " ad filings",
            //value_prefix: "",
            //value_prefix: "",
            bars: [{
                "label": "Tom Foley",
                "value": 800
            }, {
                "label": "Dannel Malloy",
                "value": 711
            }, {
                "label": "Elizabeth Esty",
                "value": 570
            }, {
                "label": "Connecticut Forward",
                "value": 437
            }, {
                "label": "Grow Connecticut",
                "value": 253
            }, {
                "label": "John McKinney",
                "value": 160
            }, {
                "label": "House Majority Pac",
                "value": 158
            }, {
                "label": "Mark Greenberg",
                "value": 150
            }, {
                "label": "Denise Nappier",
                "value": 128
            }, {
                "label": "CT FOWARD",
                "value": 116
            }]
        },
    });

});
