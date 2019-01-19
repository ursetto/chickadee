/* chickadee chicken-doc server */
/* Copyright (c) 2010, 2011 Jim Ursetto.  All rights reserved.
   License: BSD. */

/* thoughts for the future:
   $('h2').toggle(function() { $('dt.defsig').next('dd').hide(); }
                  function() { $('dt.defsig').next('dd').show(); });  // warning: deprecated in jquery 1.9
*/

/* Quick hack to make #toc and #contents visible if jQuery didn't load.
   Remove once we have reliable fallback. */
if(!window.jQuery) {
  var c = document.getElementById('contents');
  if (c) {
   c.style.display = "block";
   c.children[1].style.display = "block";
  }
  var t = document.getElementById('toc');
  if (t) {
   c.children[1].style.display = "block";
  }
}

jQuery(document).ready(function($) {
  var contents = $('#contents');
  contents.insertBefore($('#body'));
  if (contents.find('li').length < 100) {
     contents.addClass('sidebar'); /* permit display as sidebar if screen wide enough */
  }
  contents.show();

  $('input.incsearch').incsearch();
  $('#searchbox').focus();   // but only focus if primary searchbox
  $('#contents h2').click(function() {
    $(this).next().toggle();
  });
  $('#toc h2').click(function() {
    $(this).next().toggle();
  });
  $('#toc h2').next().hide();
  /* If prettify.js is loaded, use it to pretty print. */
  if (typeof prettyPrint === 'function') { prettyPrint(); }
});

/* incsearch plugin */
/* options:
   'url': AJAX request URL, defaults to ACTION of element form
   query: param name to send with query (default: element 'name' attr)
          also accepts callback taking STR and returning data: value
   delay: delay in ms before request is sent
   timeout: request timeout in ms
   isclass: class to apply to incremental search div
   selectedClass: class to apply to selected (hovered) items
   submit: selector of input to click for submit, true to click first
           submit input, false to skip submit
   Options may be passed in data-opts attribute as a JSON hash.
*/
/* deps: QueuedAjax (included inline) */
(function($) {

  /* Alarm */
  function Alarm() {}
  Alarm.prototype = {
    cancel: function() {
      if (typeof this.timeoutID == "number") {
        window.clearTimeout(this.timeoutID);
        delete this.timeoutID;
      }
    },
    schedule: function(callback, delay) {
      var self = this;
      this.cancel();
      this.timeoutID = window.setTimeout(
        function() {
          delete self.timeoutID;
          callback();
        }, delay);
    }
  };

  /* Only one outstanding prefix request is allowed at a time.
     Incoming requests are sent after DELAY ms; if another request
     comes in before that, the countdown is reset.  If a request
     comes in during the send, it is queued for retransmission
     for DELAY ms after the send completes successfully
     (any existing queued request is cancelled).
  */
  function QueuedAjax(options) {
    var sending = false;
    var enqueued_data = null;
    var alarm = new Alarm();
    var defaults = {
      url: "",
      delay: 50,
      timeout: 1500,
      type: 'GET',
      success: $.noop,
      error: $.noop
    };
    var opts = $.extend({}, defaults, options);

    return {
      send: function(data) {
        var self = this;
        if (sending) {
          /* Sending flag is not cleared on error,
             so further requests will take place.  But due to jQuery
             bug (?) in 1.4.2, no error occurs on network failure. */
          enqueued_data = data;
          return;
        }

        var ajax = function() {
          sending = true;
          /* TODO url, type and timeout can perhaps be omitted if not set
             by caller, accepting $.ajax defaults. */
          $.ajax({
            url: opts.url,
            type: opts.type,
            data: typeof data === 'function' ? data() : data,
            timeout: opts.timeout,   // FIXME: Don't want to set if undefined
            success: function(data, status, xhr) {
              opts.success(data, status, xhr);
              /* If send was enqueued during XHR, reschedule it. */
              var enq = enqueued_data;
              if (enq) {
                enqueued_data = null;
                alarm.schedule(function() { self.send(enq); },
                               opts.delay);
              }
              sending = false;
            },
            error: opts.error
          });
        };

        if (opts.delay == 0) {
          alarm.cancel();
          ajax();
        } else {
          alarm.schedule(ajax, opts.delay);
        }
      }

    };
  }

  $.fn.incsearch = function(options) {
    return this.each(function(index) {
      var opts = $.extend({}, $.fn.incsearch.defaults, options);
      var $sb = $(this);
      $.extend(opts, $sb.data('opts')); /* null attr ok */

      var $is = $('<div/>', { id: this.id ? this.id + "-" + opts.isclass
                              : opts.isclass + (index==0?'':index),   // id wrong
                              'class': opts.isclass
                            })
        .appendTo('body');
      var last_search = $sb.val();
      var query = opts.query || this.name;
      var url = opts.url || this.form.action;

      var selected = null;         // which 0-based item index is selected, or null
      var saved_value;
      var item_count = 0;          // number of items in the incsearch box

      var qajax = QueuedAjax({
        timeout: opts.timeout,
        delay: opts.delay,
        url: url,
        /* send type? */
        success: function(data, status, xhr) {
          $is.html(data);
          data == "" ? hide() : show();

          var items = $("li", $is);
          // Hack for iPad -- clickable elements must have at least a
          // no-op click event attached.
          items.click($.noop);

          item_count = items.length;
          selected = null;
        },
        error: function() { hide(); }
      });

      function hide() {
        $(window).off("resize", reposition);
        $is.hide();
      }
      function show() {
        $(window).on("resize", reposition);
        // $(window).on('orientationchange', reposition); // iPhone/iPad -- but does not work
        reposition();
        $is.show();
      }
      function reposition() {
        var pos = $sb.offset();
        // Using .offset(pos) formerly didn't work on FF and Safari;
        // it does now, but doesn't offer any benefit.  Continue to
        // set CSS directly for now.
        //// $is.offset({top: pos.top + $sb.outerHeight() + 3,
        ////             left: pos.left + 2 });
        //// $is.width($sb.innerWidth() - 4);

        $is.css({left: pos.left + 2,
                 top:  pos.top + $sb.outerHeight() + 3,
                 width: $sb.innerWidth() - 4
                });
      }
      function maybe_submit() {
        var submit = opts.submit;
        if (submit) {
          if (submit === true) {
            $('[type="submit"]', $sb[0].form).first().click();
          } else {
            $(submit).click();
          }
        }
      }

      /* init */
      $sb.keyup(function(e) {
        /* Assume up/down arrow keys are handled in keydown. */
        if (e.which == 38 || e.which == 40 || e.which == 13) return true;
        var str = $sb.val();
        if (str != last_search) {
          last_search = str;
          if (str == "") {
            hide();
          } else {
            if (typeof query === 'function') {
              qajax.send(query(str));
            } else {
              var args = {};
              args[query] = str;
              qajax.send(args);
            }
          }
        }
      });
      $sb.keydown(function(e) {
        if (item_count == 0)
          return;
        var oldsel = selected;
        if (e.which == 38) {             /* up */
          if (selected == null) {
            selected = item_count - 1;
          } else if (selected == 0) {
            selected = null;
          } else {
            selected--;
          }
        } else if (e.which == 40) {      /* down */
          if (selected == null) {
            selected = 0;
          } else if (selected >= item_count - 1) {
            selected = null;
          } else {
            selected++;
          }
        } else if (e.which == 13) {
          // Counterpart to hide on mouseup.
          hide();
          return true;
        } else {
          return true;
        }
        show();
        if (oldsel !== null) {
          var $oli = $is.find('li:eq(' + oldsel + ')');
          $oli.removeClass(opts.selectedClass);
        } else {
          saved_value = $sb.val();
        }
        if (selected !== null) {
          var $li = $is.find('li:eq(' + selected + ')');
          $li.addClass(opts.selectedClass);
          $sb.val($li.text());
        } else {
          $sb.val(saved_value);
        }
        return false;
      });

      $sb.blur(function() { hide(); });

      // Cancel mousedown; don't fire blur nor allow text selection.
      $is.mousedown(function() {
        // IE6~8 don't allow us to cancel mousedown.  Use IE specific
        // event to cancel the imminent blur instead.
        $sb.one('beforedeactivate', function() { return false; });
        return false;
      });
      $is.on("mouseup", "li", function() {
        var $t = $(this);
        $sb.val($t.text());
        hide();  // ?
        maybe_submit();
      });
      $is.on("mouseover", "li",
                   function() {
                     /* CODE DUPLICATION */
                     if (selected !== null) {
                       var $li = $is.find('li:eq(' + selected + ')');
                       $li.removeClass(opts.selectedClass);
                     } else {
                       saved_value = $sb.val();
                     }
                     $(this).addClass(opts.selectedClass);
                     selected = $(this).index();
                   });
    });
  };

  $.fn.incsearch.defaults = {
    delay: 50,
    timeout: 1500,
    isclass: 'incsearch',
    selectedClass: 'hover',
    submit: true
  };
})(jQuery);
