window.onload = init;

var last_search;

function init() {
  var sb = $('searchbox');
  last_search = sb.value;
  if (sb) {
    sb.onkeyup = function() {
      if (sb.value != last_search) {
	last_search = sb.value;
	sendGetRequest(sb.value);
      }
    };
    sb.onblur = function() {
      $('incsearch').style.visibility = "hidden";
    };
  }
}

/* FIXME: avoid overloading server here, possibly don't send
 * new request until one comes back, or establish minimum
 * interval */
function sendGetRequest(prefix) {
  var xhr = getHTTPObject();
  if (xhr) {
    xhr.onreadystatechange = function() {
      if (xhr.readyState == 4) {
	if (xhr.status == 200) {
	  var is = $('incsearch');
	  is.innerHTML = xhr.responseText;
	  is.style.visibility = (is.innerHTML == "") ? "hidden" : "visible";
	}
      }
    };
    xhr.open("GET", "/cdoc?ajax=1&prefix=" + escape(prefix), true);
    xhr.send();
  }
  return xhr;
}

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
