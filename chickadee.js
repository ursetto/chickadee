window.onload = init;

var last_search;

function init() {
  var sb = $('searchbox');
  if (sb) {
    sb.focus();
    last_search = sb.value;
    var is = $('incsearch');

    sb.onkeyup = function() {
      if (sb.value != last_search) {
	last_search = sb.value;
	prefix.send(sb.value);
      }
    };
    var hide_incsearch = function() {  // lambda lift
      is.style.visibility = "hidden";
    };
    sb.onblur = function() {
      // Blur fires before onclick, so click does not reach hidden incsearch.
      // Hack around it by delaying onblur a bit (rather than adding
      // an onclick handle to the entire document).
      window.setTimeout(hide_incsearch, 100);
    };

    function repositionIncSearch() {
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
      is.onclick = function(e) {
	e = e || window.event;
	var elt = e.target || e.srcElement;

	sb.value = textContent(elt);
	sb.focus();

	// This will submit the clicked item immediately.
	hide_incsearch();
	b = $('query-name');
	b.focus();
	b.click();
      };
    }
  }
}

/* FIXME: avoid overloading server here, possibly don't send
 * new request until one comes back, or establish minimum
 * interval */
var prefix = {
  xhr: null,
  delay: 100,
  send: function(str) {
    if (this.xhr) { return this.xhr; }
    var xhr = getHTTPObject();
    this.xhr = xhr;
    if (xhr) {
      var self = this;
      xhr.onreadystatechange = function() {
	if (xhr.readyState == 4) {
	  if (xhr.status == 200) {
	    var is = $('incsearch');
	    is.innerHTML = xhr.responseText;
	    is.style.visibility = (is.innerHTML == "") ? "hidden" : "visible";
	    self.xhr = null;
	  }
	}
      };
      xhr.open("GET", "/cdoc?ajax=1&prefix=" + escape(str), true);
      xhr.send();
    }
    return xhr;
  },
};


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

/* generic utilities */

function $(id) {
    if (typeof id == 'string') {
	var get = document.getElementById;
	return get ? document.getElementById(id) : null;
    }
    return id;
}

/* Poor man's elt.textContent, only available in DOM Level 3.
 * Just strip out all tags. */
function textContent(elt) {
  return (elt.textContent ||
	  elt.innerHTML.replace(/<[^>]+>/g, ""));  /* precompiled re */
}

function absolutePosition(elt) {
  var curleft = curtop = 0;
  if (elt.offsetParent) {
      do {
	curleft += elt.offsetLeft;
	curtop += elt.offsetTop;
      } while (elt = elt.offsetParent);
  }
  return [curleft,curtop];
}
