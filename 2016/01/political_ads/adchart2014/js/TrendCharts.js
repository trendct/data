var TrendChart = TrendChart || {};

TrendChart = function (options){

	// set up options and some required defaults.
	this.options = options;
	this.el = "#" + this.options.container_div_id;
	this.proj_name = this.options.proj_name || this.options.container_div_id;
	this.options.data.value_prefix = options.data.value_prefix || "";
	this.options.data.value_suffix = options.data.value_suffix || "";
	this.options.data.max_val = options.data.max_val || this.maxVal() * 1.1;
	//this.padding = this.options.padding || 10;
	that = this;
	
	$(window).resize(function () {that.draw(); });

	this.draw();

}

TrendChart.prototype.draw = function(){

	$(this.el).html("");
	/*
	var width = $(this.el).width();
	var height = $(this.el).height();

	$(this.el).html( 
		width
		+ " x " 
		+  height);
	*/

	// build project structure
	this.makeSkeleton();

	// add bars
	this.addBars();

	// add hover events to show/hide value labels

	var that = this;

	function showLabelWithBarName(bar_name){
		//hideLabel();
		var label_to_show = "#" + that.proj_name + "_svg_value_labels_" + Trendy.underscore(bar_name);
		$(label_to_show).show();

	}

	function showLabel(){
		showLabelWithBarName($(this).data("bar"));
	}

	function hideLabel(){
		$(".value_label").hide();
	}

	function showBiggest(){
		//$(".value_label").hide();
		showLabelWithBarName(that.biggest().label);
	}

	
	// show the biggest bar value by default
	hideLabel();
	showBiggest();

	$(".bar.trend_bar").on('mouseover', showLabel);
	$(".label.trend_label").on('mouseover', showLabel);
	$(".bar.trend_bar").on('mouseout', hideLabel);
	$(".label.trend_label").on('mouseout', hideLabel);
	$(this.el).on('mouseout', showBiggest);


}

TrendChart.prototype.makeSkeleton = function(){

	// TODO add headline
	$(this.el).append("<h1>" + this.options.title + "</h1>");

	// TODO add explainer
	$(this.el).append("<p class='lead'>" + this.options.explainer + "</p>");

	// add svg
	$(this.el).append("<svg class='trend_chart' id='" + this.proj_name + "_svg'></div>");

	// sourceline
	$(this.el).append("<div class='sourceline'>Source(s): " + this.options.sourceline + "</div>");


	// add byline
	$(this.el).append("<div class='byline'>" + this.options.byline + "</div>");


	$(this.el).append("<div style='clear:both'></div>");



}

// return the bar with the largest value
TrendChart.prototype.biggest = function(){
	var biggest = false;
	for (var i = 0; i < this.options.data.bars.length; i++){
		var bar = this.options.data.bars[i];

		if (biggest === false || biggest.value < bar.value){
			biggest = bar;
		}
	}

	return biggest;
}

// return an array of values
TrendChart.prototype.values = function (){
	var vals = [];

	for (var i = 0; i < this.options.data.bars.length; i++){
		vals.push(this.options.data.bars[i].value);
	}

	return vals.sort();

}

// return the max value
TrendChart.prototype.maxVal = function(){

	var vals = this.values();
	return this.options.data.max_val || vals[vals.length - 1];

}


TrendChart.prototype.geometry = function(){
	var ret = {};

	ret.width = $(this.el).width();
	// width of graph area
	ret.label_padding = this.options.label_padding || 5;
	ret.label_width = d3.select("#" + this.proj_name + "_svg_labels").node().getBBox().width;
	ret.bar_height = this.options.bar_height || 30;
	ret.bar_spacing = this.options.bar_spacing || 10;
	ret.bar_left_padding = this.options.bar_left_padding || ret.label_width + ret.label_padding; 
	ret.x_padding = this.options.x_padding || 10;
	ret.y_padding = this.options.y_padding || 10;
	ret.inner_width = ret.width - ret.x_padding * 2 - ret.label_width - ret.label_padding;



	return ret;
}

TrendChart.prototype.addLabel = function(bar, count){

	var geom = this.geometry();
	
	d3.select("#" + this.proj_name + "_svg g.labels")
		.append("text")
		.classed("label", true)
		.classed("trend_label", true)
		.attr("data-bar", bar.label)
		.attr("x", geom.x_padding)
		.attr("y", geom.y_padding + count * (geom.bar_height + geom.bar_spacing) + geom.bar_height / 2)
		.attr("id", this.proj_name + "_svg_labels_" + Trendy.underscore(bar.label))
		.text(bar.label);

	var bbox = d3.select("#" + this.proj_name + "_svg_labels_" + Trendy.underscore(bar.label))
	.node()
	.getBBox()

	// realign vertical center with bar
	var height = bbox.height;
	var y = bbox.height;
	var height_diff = geom.bar_height - height;
	var y = geom.y_padding
				+ (count * (geom.bar_height + geom.bar_spacing)) 
				+ (geom.bar_height / 2) 
				+ (height / 2);

	if (true){
		//console.log("height diff: " + height_diff);
		d3.select("#" + this.proj_name + "_svg_labels_" + Trendy.underscore(bar.label))
			.attr("y", 
				y
				//- (height / 2)
				);
	}

	// add value label
	d3.select("#" + this.proj_name + "_svg g.value_labels")
		.append("text")
		.classed("value_label", true)
		.classed("dark_color", true)
		.attr("data-bar", bar.label)
		.attr("x", 200)
		.attr("y", y)
		.attr("id", this.proj_name + "_svg_value_labels_" + Trendy.underscore(bar.label))
		.text(this.options.data.value_prefix + bar.value + this.options.data.value_suffix);
}

// add axes
TrendChart.prototype.addAxes = function(){
	
	var geom = this.geometry();
	var count = this.values().length;
	var y2 = count * (geom.bar_height + geom.bar_spacing) + geom.y_padding - geom.bar_spacing;
	var y1 = geom.y_padding;
	var x1 = geom.bar_left_padding + geom.x_padding;
	var x2 = x1;
	// add vertical axis
	d3.select("#" + this.proj_name + "_svg_axes")
		.append("line")
		.classed("axis", true)
		.classed("vertical", true)
		.attr("id", this.proj_name + "_svg_bars")
		.attr("x1", x1)
		.attr("y1", y1)
		.attr("x2", x2)
		.attr("y2", y2);

	// now redefine coords for horiz axis line
	x1 = x2;
	y1 = y2;
	x2 = x1 + geom.inner_width;
	y2 = y1;

	d3.select("#" + this.proj_name + "_svg_axes")
		.append("line")
		.classed("axis", true)
		.classed("horizontal", true)
		.attr("id", this.proj_name + "_svg_bars")
		.attr("x1", x1)
		.attr("y1", y1)
		.attr("x2", x2)
		.attr("y2", y2);

}


// add bars to svg
TrendChart.prototype.addBars = function (){

	d3.select("#" + this.proj_name + "_svg")
		.append("g")
		.classed("bars", true)
		.attr("id", this.proj_name + "_svg_bars");

	d3.select("#" + this.proj_name + "_svg")
		.append("g")
		.classed("labels", true)
		.attr("id", this.proj_name + "_svg_labels");

	d3.select("#" + this.proj_name + "_svg")
		.append("g")
		.classed("axes", true)
		.attr("id", this.proj_name + "_svg_axes");

	d3.select("#" + this.proj_name + "_svg")
		.append("g")
		.classed("value_labels", true)
		.attr("id", this.proj_name + "_svg_value_labels");




	for (var i = 0; i <  this.options.data.bars.length; i++){
		this.addLabel(this.options.data.bars[i], i);
	}

	this.addAxes();


	for (var i = 0; i <  this.options.data.bars.length; i++){
		this.addBar(this.options.data.bars[i], i);
	}


}

// add individual bar
TrendChart.prototype.addBar = function(bar, count){

/*
	var width = $(this.el).width();
	var inner_width = width - this.padding * 2;
	//var bar_color = bar.bar_color || "white";
	var bar_height = this.options.bar_height || 30;
	var bar_spacing = this.options.bar_spacing || 10;
	var bar_left_padding = this.options.bar_left_padding || 100; 
	var x_padding = this.options.x_padding || 10;
	var y_padding = this.options.y_padding || 10;
*/

	var max_val = this.maxVal();

	var geom = this.geometry();

	// convert value to width;
	var bar_width = d3.scale.linear().domain([0,max_val]).range([0, geom.inner_width])(bar.value);

	//console.log(geom);
	// draw bar
	d3.select("#" + this.proj_name + "_svg_bars")
		.append("rect")
		.classed("trend_chart", true)
		.classed("bar", true)
		.classed("trend_bar", true)
		.attr("data-bar", bar.label)
		.attr("width", bar_width)
		.attr("height", geom.bar_height)
		.attr("x", geom.x_padding + geom.bar_left_padding)
		.attr("y", geom.y_padding + count * (geom.bar_height + geom.bar_spacing))
		.attr("id", Trendy.underscore(bar.label.toLowerCase()));

	if (!typeof(bar.color) == "undefined"){
		d3.select("#" + Trendy.underscore(bar.label.toLowerCase()))
		.style("fill", bar.color );
	}
	// draw value text

	// resize chart div

	var g_bbox = d3.select("#" + this.proj_name + "_svg_bars").node().getBBox();
	var g_height = g_bbox.height + g_bbox.y;

	var total_height = g_height + geom.y_padding * 2;
	//console.log(g_height);
	//console.log(total_height);

	// position value label
	var value_label_id = this.proj_name + "_svg_value_labels_" + Trendy.underscore(bar.label);
	var vl_bbox = d3.select("#" + value_label_id).node().getBBox();
	var bar_bbox = d3.select("#" + Trendy.underscore(bar.label.toLowerCase())).node().getBBox();

	/** 

	value label positioning rules: 
	1. if the label fits (< width - 10) in the bar, put it toward the right of the bar
	2. else, put it outside the bar, to the right
		
	**/

	var x = bar_bbox.x + bar_bbox.width + 5;
	if (vl_bbox.width < bar_bbox.width - 10){
		x = bar_bbox.x + (bar_bbox.width) - vl_bbox.width - 5;
		// use light color if bar is "dark"
		var bar_rgb = d3.rgb(d3.select("#" + Trendy.underscore(bar.label.toLowerCase())).style("fill"));

		if (Trendy.bright(bar_rgb.r, bar_rgb.g, bar_rgb.b)){
			d3.select("#" + value_label_id)
			.classed("dark_color", true)
			.classed("light_color", false)

		}
		else {
			d3.select("#" + value_label_id)
			.classed("dark_color", false)
			.classed("light_color", true);
		}
	}

	d3.select("#" + value_label_id)
		.attr("x", x);

	// adjust svg height
	d3.select("#" + this.proj_name + "_svg")
		.attr("height", total_height);

}
