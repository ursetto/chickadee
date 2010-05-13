/* chickadee */

$(document).ready(function() {
  var sb = $('#searchbox').get(0);
  if (sb) {
    sb.focus();
    var is = $('#incsearch').get(0);

    var hide_incsearch = function() {  // lambda lift
      is.style.visibility = "hidden";
    };

    var last_search = sb.value;
    sb.onkeyup = function() {
      var str = sb.value;
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
    };

    sb.onblur = function() {
	hide_incsearch();
    };

    var repositionIncSearch = function() {
      var pos = absolutePosition(sb);
      is.style.left = pos[0] + 2 + 'px';
      is.style.top = pos[1] + sb.offsetHeight + 3 + 'px';
      is.style.width = sb.clientWidth - 4 + 'px';
    };

    /* It's possible to delay repositioning incsearch until it becomes
     * visible. */
    window.onresize = repositionIncSearch;
    repositionIncSearch();

    if (is) {
      is.onmousedown = function(e) {
	sb.onbeforedeactivate = function() {
	  // Unfortunate hack for IE6~8, which do not allow
	  // us to cancel mousedown events.  This forcibly
	  // cancels the imminent blur.
	  sb.onbeforedeactivate = null;
	  return false;
	};
	return false;   // Cancel mousedown; don't fire blur nor allow text selection
      };
      is.onmouseup = function(e) {
	e = e || window.event;
	var elt = e.target || e.srcElement;

	// Bail out if we somehow did not wind up on a
	// list item.  Ensure we take bold tag's parent.
	// (NB: since iPad requires an onclick or mousedown
	// event attached to recognize as clickable, we
	// might as well attach a mousedown on each LI
	// and get rid of this code.
	if (elt.tagName == "B") { elt = elt.parentNode;	}
	if (elt.tagName != "LI") { return true; }

	sb.value = textContent(elt);

	// This will submit the clicked item immediately.
	hide_incsearch();
	b = $('#query-name').get(0);
	// b.focus(); //?
	b.click();
      };
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
	    var is = $('#incsearch').get(0);
	    is.innerHTML = xhr.responseText;
	    is.style.visibility = (is.innerHTML == "") ? "hidden" : "visible";
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

/* Poor man's elt.textContent, only available in DOM Level 3.
 * Just strip out all tags. */
function textContent(elt) {
  return (elt.textContent ||
	  elt.innerHTML.replace(/<[^>]+>/g, ""));  /* precompiled re */
}

function absolutePosition(elt) {
  var curleft = 0;
  var curtop = 0;
  if (elt.offsetParent) {
      do {
	curleft += elt.offsetLeft;
	curtop += elt.offsetTop;
	elt = elt.offsetParent;
      } while (elt);
  }
  return [curleft,curtop];
}

