// Create the tooltips only when document ready
$(document).ready(function () {

	document.getElementById("modified.date").innerHTML = document.lastModified;

	var hash = window.location.hash 
    
	//https://plugins.jquery.com/qTip2/ - was http://qtip2.com/
	// Apply tooltip on all <a/> elements with title attributes. Mousing over
    // these elements will the show tooltip as expected, but mousing onto the
    // tooltip is now possible for interaction with it's contents.
    //$('a').each(function () {
    $('.hastip').each(function () {
        $(this).qtip({
            content: $(this).next('.tooltiptext'),
            hide: {
                fixed: true,
                delay: 300
            },
			position: {
				my: 'top left',  // Position my top left...
				at: 'bottom left', // at the bottom right of...
			},
			style: { classes: 'ToolTipClass'
			}
		});
    });

	$('.hastip').click(function(e) {
		e.stopPropagation();
		return false;
	});	
	
});

var DivName = "tips";
function toggle(itemno)
	{
	document.getElementById(itemno).style.display = (document.getElementById(itemno).style.display == "none" ) ? "" : "none";
	}
