// © 2006-2010 Joey Hess
// Redistribution and use in source and compiled forms, with or without
// modification, are permitted under any circumstances. No warranty.
//
// ikiwiki's javascript utility function library

var hooks;

// Run onload as soon as the DOM is ready, if possible.
// gecko, opera 9
if (document.addEventListener) {
	document.addEventListener("DOMContentLoaded", run_hooks_onload, false);
}
// other browsers
window.onload = run_hooks_onload;

var onload_done = 0;

function run_hooks_onload() {
	// avoid firing twice
	if (onload_done)
		return;
	onload_done = true;

	run_hooks("onload");
}

function run_hooks(name) {
	if (typeof(hooks) != "undefined") {
		for (var i = 0; i < hooks.length; i++) {
			if (hooks[i].name == name) {
				hooks[i].call();
			}
		}
	}
}

function hook(name, call) {
	if (typeof(hooks) == "undefined")
		hooks = new Array;
	hooks.push({name: name, call: call});
}

function getElementsByClass(cls, node, tag) {
        if (document.getElementsByClass)
                return document.getElementsByClass(cls, node, tag);
        if (! node) node = document;
        if (! tag) tag = '*';
        var ret = new Array();
        var pattern = new RegExp("(^|\\s)"+cls+"(\\s|$)");
        var els = node.getElementsByTagName(tag);
        for (i = 0; i < els.length; i++) {
                if ( pattern.test(els[i].className) ) {
                        ret.push(els[i]);
                }
        }
        return ret;
}
