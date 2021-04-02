// Both of these functions are sins in the world of JS, but sometimes you just
// want to create a function in a loop because writing more functions in JS
// feels certain to break the app in ways you could have never possibly anticipated

shinyjs.timeline_card_collapse = function(params) {
  // This follows some advice that I got about how to write these functions, 
  // though this is obviously not my favorite way of having to do this
  var defaultParams = {
    len: 0
  };
  params = shinyjs.getParams(params, defaultParams);
  len = params.len;
  // Now the actual js code that I want to implement
  // as with the expanded function, this one has to be mapped. This is becuase 
  // the function updating with the current position does NOT store the old position
  // values, it just moves on with the "position" variable which, in the scope of the 
  // JS event handlers, is just one number. Looping fixes an issue where you expand an
  // earlier card and then click away from it
  for (let i = 1; i < len + 1; i++) { 
    var container = $("#timeline_card_" + i);
    $('.modal-content').mousedown(function() {
            if (!container.is(event.target) &&
                !container.has(event.target).length &&
                !$('#mini_card_' + i + ' .mini_card').is(event.target)) {
                  container.animate({'height': '0vh'}, 450, function() {
                    container.css('display', 'none');
                    $("#mini_card_" + i + " .mini_card").addClass("on_screen");
                  });
                  $("#entry_timeline_bar_" + i).animate({'height': '6vh'}, 450, function() {
                    $(this).addClass('bar_block_minimized');
                  });
                } else if (container.is(event.target)) {
                  // This condition just prevents unwanted card display after a re-render
                  $('#mini_card_' + i + " .mini_card").removeClass('on_screen');
                }
            }
          );
  }
};

shinyjs.timeline_card_expand = function(params) {
  // This follows some advice that I got about how to write these functions, 
  // though this is obviously not my favorite way of having to do this
  var defaultParams = {
    len: 0
  };
  params = shinyjs.getParams(params, defaultParams);
  len = params.len;
  
  function expand(position) {
      $('#mini_card_' + position + ' .mini_card').removeClass('on_screen');
      $('#timeline_card_' + position).css({'display' : 'block', 'overflow-y': 'visible'});
      $('#timeline_card_' + position).animate({'height': '30vh'}, 450, function() {
        $('#timeline_card_' + position).css('height', '30vh');
        $('#timeline_card_' + position).css('display', 'block');
      });
      $("#entry_timeline_bar_" + position).animate({'height': '31vh'}, 450, function() {
        $("#entry_timeline_bar_" + position).removeClass('bar_block_minimized');
      });
  }
  
   // Now the actual js code that I want to implement
  // This implementation is also not perfectly ideal, however there is an intuitive 
  // explanation for why it is done this way. If you don't create a mapping of some kind,
  // then JS only runs the function on the "position" variable, which is locally declared
  // as the current position. This is no good becuase we need these observers for every 
  // mini card, not just the current one.
  
  // These two loops provide a namespace-based solution for the problem of binding too many handlers 
  // with the addition of each card. This should unbind all the previous click handlers and add fresh
  // ones with each call to the function
  if (len > 1) {
    for (let i = 1; i < len; i++) {
      $('#mini_card_' + i).off('mousedown.' + i);
      }
  }
  for (let i = 1; i < len + 1; i++) { 
    $('#mini_card_' + i).on('mousedown.' + i, function() {
      expand(i);
    });
  }
};
