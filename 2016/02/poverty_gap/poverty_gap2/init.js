jQuery(function($) {
    var counter = 4;
    var lowScore = 0;
    var tickInterval = 10;
    var highScore = 100;
    var theSub = "";
    var races = ["Limited English proficiency", "Total"];
    for (i in data) {
        var rect = $("<div class='rect'><div class='purpleBlock'></div><div class='rectLabel'>" + i + "</div><div class='greenBlock'></div></div>");
        rect.attr("area", i).css("top", (counter / 52) * 100 + "%");
        if (i == "Connecticut") {
            rect.addClass("ct");
        }
        if (i == "United States") {
            rect.addClass("nat");
        }
        $(".gapArea").append(rect);
        counter++;
    }

    setTicks();
    setRect(theSub, races[0], races[1]);



    function setRect(subject, race1, race2) {
        $(".rect").show().removeClass("backward");
        var allWidths = [];
        var obj1 = race1;
        var obj2 = race2;
        $(".rect").each(function() {
            var place = $(this).attr("area");
            if (data[place][obj1] < data[place][obj2]) {
                var left = scale(data[place][obj1]);
            } else {
                var left = scale(data[place][obj2]);
                $(this).addClass("backward");
            }
            var width = Math.abs(scale(data[place][obj1]) - scale(data[place][obj2]));

            if (race1 == race2) {
                $(this).css("left", left + "%")
                    .css("width", width + "%")
                    //.css("opacity",width/30)
                    .attr("gap", cleanNum(Math.abs(data[place][obj1] - data[place][obj2])))
                    .attr("sorter","sorting" + Math.round(left * 10000))
                     .attr("num1", data[place][obj1])
                    .attr("num2", data[place][obj2]);
                allWidths.push(Math.round(left * 10000));
            } else {
                $(this).css("left", left + "%")
                    .css("width", width + "%")
                    .attr("gap", cleanNum(Math.abs(data[place][obj1] - data[place][obj2])))
                    .attr("sorter","sorting" + Math.round(width * 10000))
                     .attr("num1", data[place][obj1])
                    .attr("num2", data[place][obj2]);
                allWidths.push(Math.round(width * 10000));
            }

            if (data[place][obj1] == null || data[place][obj2] == null) {
                $(this).css("width", "1%").css("left", "50%").hide();
            }
        });

        allWidths.sort(sortNumber).reverse();
        var uwidths = [];
        $.each(allWidths, function(i, el) {
            if ($.inArray(el, uwidths) === -1) uwidths.push(el);
        });
        allWidths = uwidths;

        var widthsCounter = 3;
        for (i in allWidths) {
            $("div[sorter=sorting" + allWidths[i] + "]").each(function() {
                $(this).css("top", (widthsCounter / 55) * 100 + "%").attr("rank", widthsCounter);
                widthsCounter++;
            });
            // if (allWidths[i] / 10000 < 100 && allWidths[i] / 10000 > -70) {

            //}
        }


        if (Number($(".ct").attr("gap")) > 100) {
            $(".gap").html("--");
        } else {
            $(".gap").html($(".ct").attr("gap"));
        }
        if (Number($(".nat").attr("gap")) > 100) {
            $(".natgap").html($(".natpub").attr("gap"));
        } else {
            $(".natgap").html($(".nat").attr("gap"));
        }

    }


    function setTicks() {
        for (i = lowScore; i < highScore; i += tickInterval) {
            var tick = $("<div class='tick'><div class='tickLabel'>" + i + "</div></div>").css("left", (i - lowScore) / (highScore - lowScore) * 100 + "%");
            $(".gapArea").append(tick);
        }
    }


    function scale(num) {
        return ((num - lowScore) / (highScore - lowScore)) * 100;
    }


    $(".race").change(function() {
        $(".tooltip").hide();
        races = [];
        $(".race").each(function() {
            var theRace = $(this).val();
            $(".natpub").removeClass("active");
            // if (theRace == "Hispanic") { theRace = "Hisp"; } 
            // else if (theRace == "Asian/Pac. Islander") { theRace = "Asian"; }
            // else if (theRace == "Am. Indian/Alaska Native") { theRace = "AmIn"; }
            // else if (theRace == "Eligible for free/reduced lunch") { theRace = "Free"; }
            // else if (theRace == "Not eligible for free/reduced lunch") { theRace = "NotFree"; }
            // else if (theRace == "English Language Learner") { theRace = "ell"}
            // else if (theRace == "Non-English Language Learner") { theRace = "notell"}	

            races.push(theRace);
        });


        setRect(theSub, races[0], races[1]);


    });


     $("body").on("click", ".rect", function(e) {
        $(".tooltip").fadeIn(200)
            .css("left", e.pageX-300)
            .css("top", $(this).css("top"));
        var n1 = $(this).attr("num1");
        var n2 = $(this).attr("num2");
        $(".tooltip").html("<div class='tooltipItem'><div class='tooltipItemColor'></div>" + Math.round(n1) + "</div><div class='tooltipItem'><div class='tooltipItemColor'></div>" + Math.round(n2) + "</div>")
    });

    function cleanNum(num) {
        if (Math.round(num * 10) / 10 % 1 == 0) {
            return Math.round(num * 10) / 10 + ".0";
        } else {
            return Math.round(num * 10) / 10;
        }
    }

    function sortNumber(a, b) {
        return a - b;
    }


});
