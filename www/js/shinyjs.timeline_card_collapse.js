shinyjs.timeline_card_collapse = function(params) {
  // This follows some advice that I got about how to write these functions, 
  // though this is obviously not my favorite way of having to do this
  var default_params = {
    position : 0
  };
  
  params = shinyjs.getParams(params, default_params);
    position = params.position;
    
  // Now the actual js code that I want to implement
  $(document).click(function() {
          var container = $("#timeline_card_" + position);
          if (!container.is(event.target) &&
              !container.has(event.target).length &&
              !$('#mini_card_' + position + ' .mini_card').is(event.target)) {
                container.animate({'height': '0vh'}, 450, function() {
                  container.css('display', 'none');
                  $("#mini_card_" + position + " .mini_card").addClass("on_screen");
                });
                $("#entry_timeline_bar_" + position).animate({'height': '6vh'}, 500, function() {
                  $(this).addClass('bar_block_minimized');
                });
              }
          }
        );
  
};