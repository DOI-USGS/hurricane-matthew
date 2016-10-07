var prcpColors = undefined;

var setColors = function() {
  $.get( "js/precip-colors.json", function( data ) {
    prcpColors = data
  });
}

var animatePrcp = function(timestep) {
  for (var i=0; i<prcpColors.length; i++) {
    var bin = i + 1;
    var selector = ".prcp-" + timestep + "-" + bin;
    // hacky, we should modify json structure
    var color = prcpColors[i][Object.keys(prcpColors[i])[0]][0];
    // switch to style for transition
    var style = {
      "fill": color,
      "transition": "fill 2s"
    };
    $(selector).css("fill", color);
  }
}
