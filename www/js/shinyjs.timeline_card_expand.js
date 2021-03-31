shinyjs.timeline_card_expand = function(params) {
  // This follows some advice that I got about how to write these functions, 
  // though this is obviously not my favorite way of having to do this
  var default_params = {
    position : 0
  };
  
  params = shinyjs.getParams(params, default_params);
    position = params.position;
    
  // Now the actual js code that I want to implement
  $('#mini_card_' + position + ' .mini_card').click(function() {
        $(this).removeClass('on_screen');
        $('#timeline_card_' + position).css('display', 'flex');
        $('#timeline_card_' + position).animate({'height': '30vh'}, 450);
        $("#entry_timeline_bar_" + position).animate({'height': '31vh'}, 500, function() {
          $(this).removeClass('bar_block_minimized');
        });
    }
  );
};