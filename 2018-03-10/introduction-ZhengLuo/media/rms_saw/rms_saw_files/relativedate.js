// © 2006-2010 Joey Hess
// Redistribution and use in source and compiled forms, with or without
// modification, are permitted under any circumstances. No warranty.
//
// Causes html elements in the 'relativedate' class to be displayed
// as relative dates. The date is parsed from the title attribute, or from
// the element content.

var dateElements;

hook("onload", getDates);

function getDates() {
	dateElements = getElementsByClass('relativedate');
	for (var i = 0; i < dateElements.length; i++) {
		var elt = dateElements[i];
		var title = elt.attributes.title;
		var d = new Date(title ? title.value : elt.innerHTML);
		if (! isNaN(d)) {
			dateElements[i].date=d;
			elt.title=elt.innerHTML;
		}
	}

	showDates();
}

function showDates() {
	for (var i = 0; i < dateElements.length; i++) {
		var elt = dateElements[i];
		var d = elt.date;
		if (! isNaN(d)) {
			elt.innerHTML=relativeDate(d);
		}
	}
	setTimeout(showDates,30000); // keep updating every 30s
}

var timeUnits = [
	{ unit: 'year',		seconds: 60 * 60 * 24 * 364 },
	{ unit: 'month',	seconds: 60 * 60 * 24 * 30 },
	{ unit: 'day',		seconds: 60 * 60 * 24 },
	{ unit: 'hour',		seconds: 60 * 60 },
	{ unit: 'minute',	seconds: 60 },
];

function relativeDate(date) {
	var now = new Date();
	var offset = date.getTime() - now.getTime();
	var seconds = Math.round(Math.abs(offset) / 1000);

	// hack to avoid reading just in the future if there is a minor
	// amount of clock slip
	if (offset >= 0 && seconds < 30 * 60 * 60) {
		return "just now";
	}

	var ret = "";
	var shown = 0;
	for (i = 0; i < timeUnits.length; i++) {
		if (seconds >= timeUnits[i].seconds) {
			var num = Math.floor(seconds / timeUnits[i].seconds);
			seconds -= num * timeUnits[i].seconds;
			if (ret)
				ret += "and ";
			ret += num + " " + timeUnits[i].unit + (num > 1 ? "s" : "") + " ";

			if (++shown == 2)
				break;
		}
		else if (shown)
			break;
	}

	if (! ret)
		ret = "less than a minute "

	return ret + (offset < 0 ? "ago" : "from now");
}
