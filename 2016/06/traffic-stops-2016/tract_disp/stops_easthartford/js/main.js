$(function () {

	var corridor_slideshow = new TrendSlideshow ({
		root_div: "corridor_slider_root",
		slider_name: "rail_corridor_122015",
		headline: "Disparity in traffic stops compared to population in East Hartford",
		caption: "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "	
			+ "Aenean non nibh orci. Nulla facilisi. Phasellus dignissim est ut "
			+ "iaculis gravida. Aenean a quam libero. Vivamus placerat nulla ut "
			+ "metus sodales faucibus. Nam commodo tempor nunc nec viverra. In "
			+ "volutpat augue a nibh mattis, sed ultricies nunc aliquet. Donec "
			+ "eget ultricies.",
		button_text: "&bull;",
		sourceline: "CCSU Institute for Municipal & Regional Policy",
		byline: "Andrew Ba Tran/TrendCT.org",
		slides: [ {
			//url: "img/corridor_alt1_ts.jpeg",
			headline: "Minorities",
			caption: "Search by <a href='http://projects.ctmirror.org/maps/index-geoc.html?d=Driverspulledoverbycensustract59201622570&s=tract' target='_blank'>address</a>.",
            html: "<img src='img/disp_EastHartford_minority.png'>",
//			html: "<img src='img/population_m.png'><br>"
//				+ "<div class='ts_key'><span class='ts_sample ts_sample_yellow'>Yellow</span> indicates chokepoint relief projects. <span class='ts_sample ts_sample_blue'>Blue</span> indicates new segments. <span class='ts_sample ts_sample_green'>Green</span> indicates new track.</div>",
		},
		{
			//url: "img/corridor_alt2_1_ts.jpeg",
			headline: "Black",
			caption: "Search by <a href='http://projects.ctmirror.org/maps/index-geoc.html?d=Driverspulledoverbycensustract59201622570&s=tract' target='_blank'>address</a>.",
			html: "<img src='img/disp_EastHartford_black.png'>",
//			html: "<img src='img/population_b.png'>"
//				+ "<div class='ts_key'><span class='ts_sample ts_sample_yellow'>Yellow</span> indicates chokepoint relief projects. <span class='ts_sample ts_sample_blue'>Blue</span> indicates new segments. <span class='ts_sample ts_sample_green'>Green</span> indicates new track.</div>",

		},
		{
			//url: "img/corridor_alt2_2_ts.jpeg",
			headline: "Hispanic",
			caption: "Search by <a href='http://projects.ctmirror.org/maps/index-geoc.html?d=Driverspulledoverbycensustract59201622570&s=tract' target='_blank'>address</a>.",
			html: "<img src='img/disp_EastHartford_hispanic.png'>",
//			html: "<img src='img/population_h.png'>"
//				+ "<div class='ts_key'><span class='ts_sample ts_sample_yellow'>Yellow</span> indicates chokepoint relief projects. <span class='ts_sample ts_sample_blue'>Blue</span> indicates new segments. <span class='ts_sample ts_sample_green'>Green</span> indicates new track.</div>",
		},
		{
			//url: "img/corridor_alt2_2_ts.jpeg",
			headline: "White",
			caption: "Search by <a href='http://projects.ctmirror.org/maps/index-geoc.html?d=Driverspulledoverbycensustract59201622570&s=tract' target='_blank'>address</a>.",
			html: "<img src='img/disp_EastHartford_white.png'>",
//			html: "<img src='img/population_h.png'>"
//				+ "<div class='ts_key'><span class='ts_sample ts_sample_yellow'>Yellow</span> indicates chokepoint relief projects. <span class='ts_sample ts_sample_blue'>Blue</span> indicates new segments. <span class='ts_sample ts_sample_green'>Green</span> indicates new track.</div>",
		},
		]
	});

	corridor_slideshow.pym_child.sendHeight();

})

