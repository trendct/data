TrendSlideshow = function (options) {

	this.options = options;

	this.pym_child = new pym.Child();

	// create basic html in root div
	$("#" + options.root_div).html(
		'<div class="trend_basic_slider_container" id="' + options.slider_name + '_ts_container">'
			+ '<div class="trend_basic_slider_headline" id="' + options.slider_name + '_ts_headline"><h3>' + options.headline + '</h3></div>'
			+ '<div class="trend_basic_slider_button_container" id="' + options.slider_name + '_ts_button_container"></div>'
			+ '<div class="trend_basic_slider_caption lead" id="' + options.slider_name + '_ts_caption">' + options.caption + '</div>'
			+ '<div class="trend_basic_slider_item_container" id="' + options.slider_name + '_ts_item_container"></div>'
			+ '<div class="trend_basic_slider_sourceline small" id="' + options.slider_name + '_ts_source"><em>Source: ' + options.sourceline + '</em></div>'
			+ '<div class="trend_basic_slider_byline small" id="' + options.slider_name + '_ts_byline">' + options.byline + '</div>'
			+ '<div style="clear:both"></div>'
		+ '</div>'
		);

	// clear toggle buttons and slides
	$("#" + options.slider_name + '_ts_item_container').html("");
	$("#" + options.slider_name + '_ts_buttons').html("");


	// add each item
	for (var i = 0; i < options.slides.length; i ++){
		slide = options.slides[i];

		$("#" + options.slider_name + '_ts_item_container').append(
			"<div class='trend_basic_slider_item' id='" + options.slider_name + "_slide_" + i +"'>"
			//+ "<img src='" + slide.url + "'>"
			+ slide.html
			+ "</div>"
		);

		$("#" + options.slider_name + '_ts_button_container').append(
			"<div class='trend_basic_slider_button' id='" + options.slider_name + "_button_" + i + "' data-slide='" + i + "' ><h6>"
			+ slide.headline
			+ "</h6></div>"
		);

		var that = this;
		// bind show hide methods
		$(".trend_basic_slider_button").on('click', function() {
			// hide all 
			//$(".trend_basic_slider_item").hide();

			// show this one slide
			//$("#" + $(this).attr("data-slide")).show();
			that.show($(this).attr("data-slide"));

		});

	}

	//$(".trend_basic_slider_item").first().show();

	$("#" + options.slider_name + '_ts_button_container').append("<div style='clear:both'></div>");
	this.show(0);

	setTimeout(	function () {
		that.pym_child.sendHeight();
	}, 500);

}

TrendSlideshow.prototype.show = function(index){

	$("#" + this.options.slider_name + "_ts_item_container .trend_basic_slider_item").hide();
	$("#" + this.options.slider_name + "_ts_button_container .trend_basic_slider_button").removeClass("selected");
	var slide = this.options.slides[index];

	console.log("add class: #" + this.options.slider_name + "_button_" + index);
	$("#" + this.options.slider_name + "_button_" + index).addClass("selected");
	$("#" + this.options.slider_name + "_slide_" + index).show();
	$("#" + this.options.slider_name + "_ts_caption").html(slide.caption);

	this.pym_child.sendHeight();


}