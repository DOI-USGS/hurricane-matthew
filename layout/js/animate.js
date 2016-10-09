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
  }
  document.getElementById('timestamp-text').firstChild.data = prcpTimes.times[timestep-1]; // zero indexed
  var darkWidth = timestep/prcpTimes.times.length;
  document.getElementById('spark-light-mask').setAttribute('x', darkWidth);
  document.getElementById('spark-light-mask').setAttribute('width', 1-darkWidth);
  document.getElementById('spark-full-mask').setAttribute('width',darkWidth);
}

$(document).ready(function() {
  setColors();
  var running = false;
  var interval;
  var intervalLength = 1000; // 1 sec
  var timestep = 1;
  var button = $("#playButton");
  button.click(function(){
    if (running) {
      clearInterval(interval);
      running = false;
      button.html("Play")
    } else {
      running = true;
      button.html("Pause")
      interval = setInterval(function() {
        if (timestep < prcpTimes.times.length) {
          animatePrcp(timestep);
          timestep++;
        } else {
          timestep = 1;
          clearInterval(interval);
          running = false;
          button.html("Play")
        }
      }, intervalLength)
    }
  });
});