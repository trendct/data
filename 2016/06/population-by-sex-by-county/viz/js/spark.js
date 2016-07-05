
$(function () {
    $(document).ready(function () {
	console.log("Document ready");
	$.getJSON("js/all_year_data_totals.json", function (data, error){
	    all_data = data;
	    make_selector();
	    make_sparks();

	    $(window).resize(function(){
		make_sparks();
	    });

	    $("#county-selector").change(make_sparks);
	    $("#gender-selector").change(make_sparks);
	    
	}).fail(function(e) {
	    console.log( "error",e.responseText );
	});
    });

});
