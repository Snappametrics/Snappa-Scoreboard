
shinyjs.timeline_card_collapse = function(position) {
  // This follows some advice that I got about how to write these functions, 
  // though this is obviously not my favorite way of having to do this
    
  // Now the actual js code that I want to implement
  $('.modal-content').click(function() {
          var container = $("#timeline_card_" + position);
          if (!container.is(event.target) &&
              !container.has(event.target).length &&
              !$('#mini_card_' + position + ' .mini_card').is(event.target)) {
                container.animate({'height': '0vh'}, 450, function() {
                  container.css('display', 'none');
                  $("#mini_card_" + position + " .mini_card").addClass("on_screen");
                });
                $("#entry_timeline_bar_" + position).animate({'height': '6vh'}, 450, function() {
                  $(this).addClass('bar_block_minimized');
                });
              }
          }
        );
  
};

shinyjs.timeline_card_expand = function(params) {
  // This follows some advice that I got about how to write these functions, 
  // though this is obviously not my favorite way of having to do this
  var defaultParams = {
    position: 0
  };
  params = shinyjs.getParams(params, defaultParams);
  position = params.position;
  
  // Now the actual js code that I want to implement
  $('.modal-content').mousedown(function() {
    var mini_card =  $('#mini_card_' + position + ' .mini_card');
    if (mini_card.is(event.target)) {
        mini_card.removeClass('on_screen');
        $('#timeline_card_' + position).css('display', 'block');
        $('#timeline_card_' + position).animate({'height': '30vh'}, 450, function() {
          $(this).css('height', '30vh');
        });
        $("#entry_timeline_bar_" + position).animate({'height': '31vh'}, 450, function() {
          $(this).removeClass('bar_block_minimized');
        });
    }
  });
};
