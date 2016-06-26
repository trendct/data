function bind_stuff(){
    $(".form-control").change(function(){
	var year = $("#year-selector option:selected").val();
	var county = $("#county-selector option:selected").val();
	console.log(county, year);

	make_chart(year_categories[year],
		   county,
		   all_data[county][year]);
    });
}



$(function () {
    $(document).ready(function () {
	console.log("Document ready");
	$.getJSON("js/all_year_data.json", function (data, error){
	    all_data = data;
	    make_selector();
	    make_chart("2015",
	    	       "Fairfield County",
	    	       data["Fairfield County"]["8"]);
	    bind_stuff();
	    
	}).fail(function(e) {
	    console.log( "error",e.responseText );
	});
    });

});
