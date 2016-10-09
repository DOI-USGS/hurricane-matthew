var prcpColors = undefined;
var prcpTimes = undefined;

var setColors = function() {
  $.get( "js/precip-colors.json", function( data ) {
    prcpColors = data
  });
  $.get( "js/times.json", function( data ) {
    prcpTimes = data
  });
}

var animatePrcp = function(timestep) {
  for (var i=0; i<prcpColors.length; i++) {
    var bin = i + 1;
    var selector = ".prcp-" + timestep + "-" + bin;
    var stormDot = ".storm-dot";
    var color = prcpColors[i];
    // switch to style for transition
    var style = {
      "fill": color,
      "transition": "fill 2s"
    };
    $(selector).css("fill", color);
    $(stormDot).css("opacity", "0");
    var storm = document.getElementById("storm-" + timestep);
    if (!storm){
    } else {
      storm.setAttribute('style','opacity: 1.0;')
    }
    document.getElementById('timestamp-text').firstChild.data = prcpTimes.times[timestep-1]; // zero indexed
  }
}
