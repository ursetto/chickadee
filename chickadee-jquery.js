/* chickadee chicken-doc server */
/* Copyright (c) 2010 Jim Ursetto.  All rights reserved.
   License: BSD. */

/* thoughts for the future:
   $('h2').toggle(function() { $('dt.defsig').next('dd').hide(); }
                  function() { $('dt.defsig').next('dd').show(); });
*/

$(document).ready(function() {
  var sb = $('#searchbox');

  if (sb.length) {
    sb.focus();
    var is = $('#incsearch');

    var hide_incsearch = function() {  // lambda lift
      is.hide();
    };

    var last_search = sb.val();
    sb.keyup(function() {
      var str = sb.val();
      if (str != last_search) {
	last_search = str;
	if (str == "") {
	  hide_incsearch();
	} else {
	  // send does not currently require you to determine
	  // the value of the prefix at callback time.
	  prefix.send(function() { return str; });
	}
      }
    });

    sb.blur(function() { hide_incsearch(); });

    var repositionIncSearch = function() {
      var pos = sb.offset();
      // Using .offset(pos) causes jumpiness on FF and
      // wrong position on Safari.
      is.css({left: pos.left + 2,
              top:  pos.top + sb.outerHeight() + 3,
              width: sb.innerWidth() - 4
             });
    };

    /* It's possible to delay repositioning incsearch until it becomes
     * visible. */
    $(window).resize(function(e) { repositionIncSearch(); });
    repositionIncSearch();
    
    // Cancel mousedown; don't fire blur nor allow text selection.
    is.mousedown(function(e) {
      // IE6~8 don't allow us to cancel mousedown.  Use IE specific
      // event to cancel the imminent blur instead.
      $(sb).one('beforedeactivate', function() { return false; });
      return false;
    });
    is.delegate("li", "mouseup", function(e) {
      // (NB: iPad requires onclick/mousedown event on each LI.
      // We did this below; so this delegation may be pointless.
      var t = $(this);
      sb.val(t.text());
      hide_incsearch();  // ?
      $('#query-name').click();
    });
  }
});

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

var prefix = {
  sending: false,
  uri: "/cdoc/ajax/prefix",    // should be configurable
  delay: 50,                   // should be configurable
  incsearch: '#incsearch',
  timeout: 1500,
  alarm: new Alarm(),

  send: function(cb) {
    /* Only one outstanding prefix request is allowed at a time.
       Incoming requests are sent after DELAY ms; if another request
       comes in before that, the countdown is reset.  If a request
       comes in during the send, it is queued for retransmission
       for DELAY ms after the send completes successfully
       (any existing queued request is cancelled).
    */
    var self = this;
    if (self.sending) {
      /* Sending flag is not cleared on error,
         so further requests will take place.  But due to jQuery
         bug (?) in 1.4.2, no error occurs on network failure. */
      self.enqueued_cb = cb;
      return;
    }

    var ajax = function() {
      self.sending = true;
      $.ajax({
        url: self.uri,
        timeout: self.timeout,   // FIXME: Don't want to set if undefined
        type: 'GET',
        data: { q: cb() },
        success: function(data, status, xhr) {
          var is = $(self.incsearch);
          is.html(data);
          // Hack for iPad -- clickable elements must have at least a
          // no-op click event attached.  Should move this to callback
          $("li", is).click($.noop);
          data == "" ? is.hide() : is.show();
	  /* If send was enqueued during XHR, reschedule it. */
	  var ecb = self.enqueued_cb;
	  delete self.enqueued_cb;
	  if (ecb) {
	    self.alarm.schedule(function() { self.send(ecb); },
                                self.delay);
	  }
          self.sending = false;
        },
        error: function() { $(self.incsearch).hide(); }
      });
    };

    if (this.delay == 0) {
      this.alarm.cancel();
      ajax();
    } else {
      this.alarm.schedule(ajax, this.delay);
    }
  }
};
