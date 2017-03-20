$.fn.tooltip = function() {
	$(this).live('click', function() {
		$('span.showing').find('.toolTipWrapper').fadeOut(300);
		this.tip = $(this).find('span').html();
		$(this).append('<div class="toolTipWrapper"><div class="toolTipMid">'+this.tip+'</div></div>');
		this.width = $(this).width();
		if ($(this).hasClass('topRight')) {
			$(this).addClass('showing').find('.toolTipWrapper').css({right: this.width + 20}).fadeIn(300);
		} else {
			$(this).addClass('showing').find('.toolTipWrapper').css({left: this.width + 20}).fadeIn(300);
		}
	});

	$('body').click(function(e){
		$('span.showing').find('.toolTipWrapper').fadeOut(300);
	});
}

$('.dismiss').live('click', function() {
	$(this).closest('.infoBox').slideUp(300);
	var result = ($.post($(this).closest('form').attr('action')).responseText);

	if (result.error == true) {
		alert('Failed to save tip visibility preferences.');
	}

	return false;
});

/* Register global error handler */
window.onerror = function (message, uri, line) {
	var fullMessage = message + "\n at " + uri + ": " + line
	/* alert(fullMessage); */
	/* Let the browser take it from here*/
	return false;
}

function otliam() {
	var e = $(arguments[0]);
	for (var i=arguments.length-1; i > 0; i--) {
		e.href += arguments[i];
		e.innerHTML += (arguments[i]);
	}
}

var registerHideShowToggle = function() {
	// setup hide/show buttons
	$('.hideShowToggle').unbind('click').click(function(e) {
		// don't let the anchor go to the href
		e.preventDefault();
		// get the class that should be toggled
		var cls = $(':input:hidden:first', $(this)).val();
		// toggle the button
		$('span', $(this)).toggle();
		$(this).toggleClass('open').toggleClass('close');
		// toggle the various elements
		$('.' + cls).slideToggle('normal');
		// focus the first element after the clicked button
		$(':input:first', $(this).parent().next()).focus();
		return false;
	});
}
$(document).ready( registerHideShowToggle );

// Inspired by base2 and Prototype
(function(){
	var initializing = false, fnTest = /xyz/.test(function(){xyz;}) ? /\b_super\b/ : /.*/;
	
	// The base Class implementation (does nothing)
	this.Class = function(){};
	
	// Create a new Class that inherits from this class
	Class.extend = function(prop) {
		var _super = this.prototype;
		
		// Instantiate a base class (but only create the instance,
		// don't run the init constructor)
		initializing = true;
		var prototype = new this();
		initializing = false;
		
		// Copy the properties over onto the new prototype
		for (var name in prop) {
			// Check if we're overwriting an existing function
			prototype[name] = typeof prop[name] == "function" &&
			typeof _super[name] == "function" && fnTest.test(prop[name]) ?
			(function(name, fn){
				return function() {
					var tmp = this._super;
					
					// Add a new ._super() method that is the same method
					// but on the super-class
					this._super = _super[name];
					
					// The method only need to be bound temporarily, so we
					// remove it when we're done executing
					var ret = fn.apply(this, arguments);       
					this._super = tmp;
					
					return ret;
				};
			})(name, prop[name]) :
			prop[name];
		}
		
		// The dummy class constructor
		function Class() {
			// All construction is actually done in the init method
			if ( !initializing && this.init )
			this.init.apply(this, arguments);
		}
		
		// Populate our constructed prototype object
		Class.prototype = prototype;
		
		// Enforce the constructor to be what we expect
		Class.constructor = Class;
		
		// And make this class extendable
		Class.extend = arguments.callee;
		
		return Class;
	};
})();
