// © 2006-2010 Joey Hess
// Redistribution and use in source and compiled forms, with or without
// modification, are permitted under any circumstances. No warranty.
//
// Uses CSS to hide toggleables, to avoid any flashing on page load. The
// CSS is only emitted after it tests that it's going to be able
// to show the toggleables.
if (document.getElementById && document.getElementsByTagName && document.createTextNode) {
	document.write('<style type="text/css">div.toggleable { display: none; }</style>');
	hook("onload", inittoggle);
}

function inittoggle() {
	var as = getElementsByClass('toggle');
	for (var i = 0; i < as.length; i++) {
		var id = as[i].href.match(/#(\w.+)/)[1];
		if (document.getElementById(id).className == "toggleable")
			document.getElementById(id).style.display="none";
		as[i].onclick = function() {
			toggle(this);
			return false;
		}
	}
}

function toggle(s) {
	var id = s.href.match(/#(\w.+)/)[1];
	style = document.getElementById(id).style;
	if (style.display == "none")
		style.display = "block";
	else
		style.display = "none";
}
