/* chickadee */

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
    
    if (is.length) {
      var sbelt = sb.get(0);
      var deact = function() { sbelt.onbeforedeactivate = null; return false; };  //oneshot
      // Cancel mousedown; don't fire blur nor allow text selection.
      is.mousedown(function(e) {
        // Unfortunate hack for IE6~8, which do not allow
	// us to cancel mousedown events.  This forcibly
	// cancels the imminent blur (but allows text select).
	sbelt.onbeforedeactivate = deact;
	return false;
      });
      is.delegate("li", "mouseup", function(e) {
	// (NB: iPad requires onclick/mousedown event individually
        // attached to recognize as clickable!
        var t = $(this);
	sb.val(t.text());
	hide_incsearch();  // ?
	$('#query-name').click();
      });
    }
  }
});

var prefix = {
  xhr: null,
  uri: "/cdoc/ajax/prefix",    // should be configurable
  delay: 50,                   // should be configurable
  cancel: function() {
    if (typeof this.timeoutID == "number") {
      window.clearTimeout(this.timeoutID);
      delete this.timeoutID;
    }
  },
  schedule: function(cb) {
    var self = this;
    this.cancel();
    this.timeoutID = window.setTimeout(
      function() {
	delete self.timeoutID;
	cb();
      }, this.delay);
  },
  send: function(cb) {
    var self = this;
    if (this.xhr) {
      self.enqueued_cb = cb;
      return this.xhr;
    }
    var xhr = getHTTPObject();
    this.xhr = xhr;
    if (xhr) {
      xhr.onreadystatechange = function() {
	if (xhr.readyState == 4) {
	  if (xhr.status == 200) {
            var is = $('#incsearch');
            is.html(xhr.responseText);
            xhr.responseText == "" ? is.hide() : is.show();
	    self.xhr = null;

	    /* If send was enqueued during XHR, reschedule it. */
	    var ecb = self.enqueued_cb;
	    delete self.enqueued_cb;
	    if (ecb) {
	      self.schedule(function() { self.send(ecb); });
	    }
	  }
	  // Note that, if XHR does not return successfully, the xhr
	  // object is never deleted and future callbacks will just
	  // be enqueued instead of sent.
	}
      };
//    this.cancel();  // We need to cancel any outstanding timeout (e.g.
                      // from an enqueued callback), whether implicitly
		      // via another schedule() call, or explicitly via cancel().
      this.schedule(
        function() {
          var pfx = cb();
	  xhr.open("GET", self.uri + "?q=" + encodeURIComponent(pfx), true);
	  xhr.send();
      });
    }
    return xhr;
  }
};


/* generic utilities */

function getHTTPObject() {
  var xhr = false;
  if (window.ActiveXObject) {
    try {
      xhr = new ActiveXObject("Msxml2.XMLHTTP");
    } catch(e) {
      try {
        xhr = new ActiveXObject("Microsoft.XMLHTTP");
      } catch(e) {
        xhr = false;
      }
    }
  } else if (window.XMLHttpRequest) {
    try {
      xhr = new XMLHttpRequest();
    } catch(e) {
      xhr = false;
    }
  }
  return xhr;
}

