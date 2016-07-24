$(function() {

    var chart = new TrendChart({
        container_div_id: "chart_container",
        sourceline: "Federal Communications Commission, politicaladsleuth.com",
        title: "2012 FCC ad filings in Connecticut",
        explainer: "The 10-most filings at local tv stations.",
        byline: "Andrew Ba Tran / TrendCT.org",
        bar_spacing: 1,
        bar_height: 40,
        x_padding: 20,
        //override horizontal alignment relative to svg left:0
        //bar_left_padding: 60,
        data: {
            // override max scale
            max_val: 700,
            value_suffix: " ad filings",
            //value_prefix: "",
            //value_prefix: "",
            bars: [{
                "label": "Linda McMahon",
                "value": 696
            }, {
                "label": "Chris Murphy",
                "value": 308
            }, {
                "label": "Democratic Congressional Campaign Committee",
                "value": 270
            }, {
                "label": "Elizabeth Esty",
                "value": 121
            }, {
                "label": "Andrew Roraback",
                "value": 119
            }, {
                "label": "MAJORITY PAC",
                "value": 103
            }, {
                "label": "Joseph Courtney",
                "value": 73
            }, {
                "label": "CT FUTURE PAC",
                "value": 63
            }, {
                "label": "Patriot Majority",
                "value": 62
            }, {
                "label": "GOVERNMENT INTEGRITY FUND",
                "value": 61
            }]
        },
    });

});
