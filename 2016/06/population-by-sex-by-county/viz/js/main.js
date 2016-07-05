
// definitions here
// http://www.census.gov/popest/data/counties/asrh/2015/files/CC-EST2015-ALLDATA.pdf

var all_data = false;
var totals_data = false;

var county_categories = ['Fairfield County', 'Hartford County', 'Litchfield County',
			 'Middlesex County', 'New Haven County', 'New London County',
			 'Tolland County', 'Windham County'];

var age_categories = [
    'Total',
    '0-4',
    '5-9',
    '10-14',
    '15-19',
    '20-24',
    '25-29',
    '30-34',
    '35-39',
    '40-44',
    '45-49',
    '50-54',
    '55-59',
    '60-64',
    '65-69',
    '70-74',
    '75-79',
    '80-84',
    '85+'];

var year_categories = [false,false,false,"2010","2011","2012","2013","2014","2015"];


function make_selector(){
    for (i in year_categories){
	var year = year_categories[i];
	if (year == false) continue;
	
	$("#year-list").append("<option value='" + i +"'>"
			       + year + "</option>");
    }

    for (i in county_categories){
	var county = county_categories[i];

	$("#county-list").append("<option value='" + county + "'>"
				 + county + "</option>");
    }
}

function make_chart(year, county, data){
    $('#container').highcharts({
        chart: {
	    style : {
		fontFamily : "Lato"
	    },
            type: 'bar'
        },
	credits: {
	    href:"http://trendct.org",
	    text: "JAKE KARA / TRENDCT.ORG"
	},
        title: {
            text: "" // county + ", " + year 
        },
        subtitle: {
            text: county + ", " + year 
        },
        xAxis: [{
            categories: age_categories,
            reversed: false,
            labels: {
                step: 1
            }
        }, { // mirror axis on right side
            opposite: true,
            reversed: false,
            categories: age_categories,
            linkedTo: 0,
            labels: {
                step: 1
            }
        }],
        yAxis: {
            title: {
                text: null
            },
            labels: {
                formatter: function () {
                    return Math.abs(this.value) + '%';
                }
            }
        },

        plotOptions: {
	    bar: {
		animation: false,
		tooltip: {
		},
	    },
            series: {
                stacking: 'normal'
            }
        },

        tooltip: {
            formatter: function () {
                return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
                    'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 1) + "%";
            }
        },

        series: [{
            name: 'Men',
            data: data["MALE"].slice(1)
        }, {
            name: 'Women',
            data: data["FEMALE"].slice(1)
        }]
    });
}

function spark(data, div){
    console.log(div);
    console.log(data);
    data = data.map(function (a){
	return Math.abs(a);
    });
    $("#" + div).sparkline(data, {width: "100%",height:"20",
				  tooltipContainer:"#" + div + "-label",
				  tooltipClassname:"spark-tooltip",
				  numberFormatter:function(tip){
				      console.log(tip);
				      fmt_tip = Math.abs(tip).toLocaleString();
				      $(".hover-label").html("");
				      $("#" + div + "-label").html(fmt_tip);
				      return "";
				  }
				 });
}

function make_spark_data(county){
    var data = {"MALE":{},
		"FEMALE":{}}


    for (i in age_categories){
	
	var age = age_categories[i];
	console.log(i, age);
	
	
	data["MALE"][i] = [];
	data["FEMALE"][i] = [];
	
	for (j in all_data[county]){
	    console.log(all_data[county][j]["MALE"][i]);
	    data["MALE"][i].push(all_data[county][j]["MALE"][i]);
	    data["FEMALE"][i].push(all_data[county][j]["FEMALE"][i]);
	}

    }

    return data;
}

function make_sparks(){

    var county = $("#county-selector option:selected").val();
    var gender = $("#gender-selector option:selected").val();
    var data = make_spark_data(county);

    $("#spark_body").html("");

    for (i = 0 ; i < age_categories.length; i++){
	age_label = age_categories[i];

	console.log(gender);
	$("#spark_body").append("<div class='row short-row'>"
				+ "<div class='col-xs-3'><strong>" + age_label + "</strong></div>"
				+ "<div class='col-xs-6' id='" + i + "-SPARK'>SPARK</div>"
				// + "<div class='col-xs-4'>"
				// + Math.abs(Math.round(data[gender][i][0] * 10)) / 10 + "% to "
				// + Math.abs(Math.round(data[gender][i][7] * 10)) / 10
				// + "%</div>"
				+ "<div id='" + i + "-SPARK-label' class='hover-label col-xs-3'>"

				+ "</div>"

				+ "</div>");
    }

    for (i in age_categories){
	console.log("going to spark");
	spark(data[gender][i], i + "-SPARK");
	// spark(data["MALE"][i], i + "-FEMALE");
    }

    
}
