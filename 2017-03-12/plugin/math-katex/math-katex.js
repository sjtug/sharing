/* global Reveal, katex */

/**
 * A plugin that renders mathematical formulas inside reveal.js slides using
 * [KaTeX](https://github.com/Khan/KaTeX]).
 *
 * Calls `katex.renderToString` on DOM elements and regex matches and does some
 * error handling.
 *
 *
 * (Supports loading KaTeX from the web as a fallback solution if it is not
 * found locally. This is currently not needed, as the build includes KateX.)
 *
 * @author Johannes Schmitz
 */
window.RevealMath = window.RevealMath || (function() {
	'use strict';

	// --- Options and defaults ------------------------------------------------

	var options = Reveal.getConfig().math || {};

	options.katexScript     = options.katexScript     || 'plugin/math-katex/lib/katex-0.6.0/katex.min.js';
	options.katexStylesheet = options.katexStylesheet || 'plugin/math-katex/lib/katex-0.6.0/katex.min.css';

	if ( options.ignoredElements ) {
		options.ignoredElements = options.ignoredElements
			.map(function ( x ) {
				return x.toUpperCase;
			});
	} else {
		options.ignoredElements = [ 'PRE', 'CODE' ];
	}

	if ( options.enableGlobally === undefined ) {
		options.enableGlobally = true;
	}

	if ( options.notificationsEnabled === undefined ) {
		options.notificationsEnabled = true;
	}


	// Hard-coded settings:

	var defaults = {
		// Will try to fetch KaTeX from these URLs, if no local copy can be
		// found
		cdn: {
			script:     'https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.js',
			stylesheet: 'https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.css'
		},

		formulaClass: 'formula',
		ignoredClass: 'math-ignored'
	};

	// -------------------------------------------------------------------------


	function addStylesheet() {
		// - To fix a CSS issue: Prevent reveal.js from overriding the font of
		//   KaTeX elements.
		//
		// - To highlight errors (`.math-katex-error`)

		var stylesheet = document.createElement( 'style' );

		stylesheet.innerHTML = [
			'.reveal .katex { font-family: KaTeX_Main; }',
			'.reveal .math-katex-error { color: red }',

			'.reveal .katex.display { display: block }',

			'.reveal .katex.inline:not(first) {',
			'    margin-left: 0.35em;',
			'}'
		]
		.join( '\n' );

		document.head.appendChild( stylesheet );
	}



	// --- TeX mode (dollars) --------------------------------------------------

	/**
	 * Replaces a formula within `$ … $` (inline) or `$$ … $$` (display).
	 */
	var replaceFormulaTex = (function() {

		// Pre-compiled regular expressions

		var regexInline   = /([^\\])\$([^\$]*)\$/g;       // $ … $
		var regexDisplay  = /([^\\])\$\$([^\$]*)\$\$/g;   // $$ … $$

		// For workarounds:
		var regexCssClass = /<span class="katex">/;       // Used to add classes


		return function ( mode, markup ) {
			var regex;

			if ( mode === 'inline'  ) {
				regex = regexInline;
			}
			else if ( mode === 'display' ) {
				regex = regexDisplay;
			}
			else {
				throw new Error( 'Invalid mode: ' + mode );
			}


			function replacer( _, lookbehind, group, offset ) {
				// (Depends on mode, thus nested.)

				group = replaceEntities( group );

				if ( mode === 'display' ) {
					var prefix = '\\displaystyle {';
					offset += 2;    // $$
					offset -= prefix.length;

					group = prefix + group + '}';
				}
				else {
					offset++;    // $
				}

				var markup;
				try {
					markup = lookbehind + katex.renderToString( group );
				}
				catch ( error ) {
					correctErrorPosition( error, offset );  // mutates the error
					throw error;
				}


				// Add a class for the mode to the root element
				markup = markup.replace(
					regexCssClass,
					'<span class="katex ' + mode + '">'
				);

				return markup;
			}

			markup = markup.replace( regex, replacer );
			return markup;
		};

	})();


	var replaceEntities = (function () {
		var regexEntityLt = /&lt;/g;
		var regexEntityGt = /&gt;/g;
		var regexEntityAmpersand = /&amp;/g;

		return function ( markup ) {
			// Replace `&lt;` and `&gt;`
			return markup
				.replace( regexEntityLt, '<' )
				.replace( regexEntityGt, '>' )
				.replace( regexEntityAmpersand, '&' );
		};
	})();


	/**
	 * Unescapes `\$` to `$` (for `tex` mode).
	 */
	var unescapeDollarSign = (function() {

		var regexUnescape = /\\\$/g;

		return function ( text ) {
			return text.replace( regexUnescape, '$' );
		};
	})();



	// --- Formula error handling ----------------------------------------------

	/**
	 * Change the `position` property of an error object thrown by KaTeX to get
	 * a more accurate error position. Mutates the passed object.
	 */
	function correctErrorPosition( error, offset ) {
		offset = offset || 0;
		error.position += offset;

		// Look for "… got '…' …" error message to adjust the position
		// (this will probably be broken by KaTeX updates).

		var matches = error.message.match( /got\s'(.*?)'/ );
		if ( matches !== null ) {
			// Text: "… got '…' …"
			error.position -= matches[1].length;
		}
	}


	/**
	 * Handles a KaTeX error by showing a more meaningful error message and
	 * highlighting the error on the slide.
	 *
	 * Wraps the error with a `span` with class `math-katex-error`. Notifies the
	 * user, but only for the first error.
	 */
	var handleError = (function() {

		var showedError = false;

		return function( error, formulaElement ) {

			var e = formulaElement;
			var slideNumber = getSlideNumber( e );

			// Wrap
			var s = e.innerHTML;
			e.innerHTML = s.slice( 0, error.position ) +
			              '<span class="math-katex-error">' +
			              s.slice( error.position ) +
			              '</span>';

			// Just show a `window.alert`
			if ( options.notificationsEnabled && !showedError ) {
				window.alert(
					'Formula on slide ' + slideNumber +
					' contains an error:\n\n' + error.message
				);

				showedError = true;
			}

			throw new Error(
				'Formula error on slide ' + slideNumber + ': ' +
				error.message + '\n' +
				error.stack
			);
		};
	})();


	/**
	 * Returns the slide number for a `section` DOM element.
	 *
	 * @param {Node} element    An element on a slide, or the `section` itself.
	 */
	function getSlideNumber( element ) {
		var presentation = document.querySelector( '.reveal .slides' );
		var slides = presentation.querySelectorAll( 'section' );
		var slideElement = getParentSlide( element );

		for ( var i = 0; i < slides.length; i++ ) {
			if ( slides[i] === slideElement ) {
				return i;     // -1 here because the title slide is not counted.
			}
		}

		return null;
	}


	function getParentSlide( element ) {

		if ( element.nodeName === 'SECTION' ) {
			return element;
		}

		do {
			element = element.parentNode;
		} while ( element && element.nodeName !== 'SECTION' );

		return element;
	}



	// --- Perform replacements ------------------------------------------------


	/**
	 * Replaces formulas in all slides: `$…$` / `$$…$$` or wrapped in elements
	 * with class `formula` or `math`.
	 */
	function replaceFormulas() {

		// Elements that wrap formulas explictly
		var wrappedElements = document.querySelectorAll( '.formula, .math' );

		// Slides that contain `$…$` or `$$…$$`
		var texSlideElements = document.querySelectorAll(
			options.enableGlobally === true ?
			'.reveal section' :
			'.reveal section[data-math]'
		);


		/**
		 * Tests if an element should be ignored for formula replacements.
		 */
		function isIgnored( element ) {

			var e = element;

			var isIgnoredElement =
				options.ignoredElements.indexOf( e.nodeName ) !== -1;

			// Elements may be marked as ignored with a class
			var isIgnoredClass =
				e.classList.contains( defaults.ignoredClass );

			// Also look for the ignored class on the parent (non-recursive)
			var parent = e.parentNode;
			if ( parent ) {
				isIgnoredClass = isIgnoredClass || parent.classList.contains(
					defaults.ignoredClass
				);
			}

			// Ignore script elements, unless they are templates (e.g. Markdown)
			var isTemplate = e.getAttribute( 'type' ) === 'text/template';
			var isScript = e.nodeName === 'SCRIPT' && !isTemplate;

			return isIgnoredElement || isIgnoredClass || isScript;
		}


		/**
		 * `forEach` for a set of DOM elements, excludes ignored elements.
		 */
		function each( arrayLike, f ) {
			for ( var i = 0; i < arrayLike.length; i++ ) {
				var element = arrayLike[i];

				if ( !isIgnored( element ) ) {
					f( element, i );
				}
			}
		}


		// Render <… class="formula"> … <…/> formulas

		each( wrappedElements, function ( e ) {

			var formula = e.textContent;
			var offset = 0;    // For error-position correction

			if ( e.classList.contains( 'display' ) ) {
				// Prepend KaTeX instruction, correct offset

				var prefix = '\\displaystyle {';
				formula = prefix + formula + '}';
				offset -= prefix.length;
			}
			else {
				e.classList.add( 'inline' );     // ensure class
			}

			try {
				e.innerHTML = katex.renderToString( formula );

				e.classList.add( 'formula' );    // In case it was selected
				                                 // with `.math`.

				// KaTeX adds a wrapper element, but we already have one, so
				// unwrap:
				e.classList.add( 'katex' );
				e.innerHTML = e.querySelector( '.katex' ).innerHTML;
			}
			catch ( error ) {
				correctErrorPosition( error, offset );  // mutates the error
				handleError( error, e );
			}
		});


		// Render `$…$` and `$$…$$` formulas
		// (must run after wrapped elements replacements to not replace twice)

		each( texSlideElements, function ( e ) {

			try {
				e.innerHTML = replaceFormulaTex( 'display', e.innerHTML );
				e.innerHTML = replaceFormulaTex( 'inline',  e.innerHTML );
				e.innerHTML = unescapeDollarSign( e.innerHTML );

				// Add a class to the created rendered elements (but keep class
				// `katex`, KaTeX needs this for its own CSS)

				var renderedFormulas = e.querySelectorAll( '.katex' );

				each( renderedFormulas, function ( forumula ) {
					forumula.classList.add( defaults.formulaClass );
				});
			}
			catch ( error ) {
				handleError( error, e );
			}
		});
	}


	// --- Script and stylesheet loading ---------------------------------------

	function loadAsset( options ) {
		// (Adopted from `math` plugin)

		if ( typeof options === 'string' ) {
			var url = options;
			options = {};
			options.url = url;
		}

		var type = options.url.split( '.' ).slice( -2 ).indexOf( 'js' ) !== -1 ?
		           'script' :
		           'stylesheet';

		var head = document.querySelector( 'head' );
		var element;

		if ( type === 'script' ) {
			var script = document.createElement( 'script' );
			script.type = 'text/javascript';
			script.src = options.url;
			element = script;
		}
		else if ( type === 'stylesheet' ) {
			var link = document.createElement( 'link' );
			link.rel = 'stylesheet';
			link.href = options.url;
			element = link;
		}

		// Wrapper for callback to make sure it only fires once
		var finish = function() {
			if ( typeof options.onLoad === 'function' ) {
				options.onLoad.call();
				options.onLoad = null;
			}
		};

		element.onload = finish;
		element.onerror = options.onError;

		// IE
		element.onreadystatechange = function() {
			if ( this.readyState === 'loaded' ) {
				finish();
			}
		};

		// Normal browsers
		head.appendChild( element );
	}



	// --- Load KaTeX and run the plugin ---------------------------------------

	// (Could really use promises. Avoided another dependency for now … )


	/**
	 * Loads KaTeX by first trying to load it locally (from `lib/katex`), then
	 * from a CDN as a fallback.
	 */
	function loadKatex( callback ) {

		if ( window.katex ) {
			callback();    // Already loaded.
			return;
		}

		// Try to load it from `lib/katex` (or another configured path)
		loadAsset({
			url: options.katexScript,

			onLoad: function() {
				loadAsset( options.katexStylesheet );   // Load CSS in parallel
				callback();
			},

			onError: tryToGetFromCdn
		});


		function tryToGetFromCdn() {

			loadAsset( defaults.cdn.stylesheet );       // Load CSS in parallel

			loadAsset({
				'url': defaults.cdn.script,

				onLoad: function() {
					console.log( 'Loaded KaTeX from the CDN' );
					callback();
				},

				onError: function() {
					throw new Error(
						'Could not load KaTeX from `lib` directory or CDN.'
					);
				}
			});
		}
	}


	function runPlugin() {

		addStylesheet();
		replaceFormulas();

		Reveal.layout();    // Update the slide layout

		// Trigger `math-rendered` event
		var event = document.createEvent( 'HTMLEvents', 1, 2 );
		event.initEvent( 'math-rendered', true, true );
		document.querySelector( '.reveal' ).dispatchEvent( event );

	}

	loadKatex( runPlugin );

})();
