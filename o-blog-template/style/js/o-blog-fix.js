/*
 * Fix some org-mode html export
 */

$(document).ready(
    function() {

	/* create the top menu bar */
	var dropdown = $('.navbar-inner .nav-collapse > ul li ul').parent();
	dropdown.parent().addClass("nav");

	/* find sub menu items */
	dropdown.addClass("dropdown");

	/* and add dropdown features */
	dropdown.find('> a').addClass('dropdown-toggle')
	    .attr("data-toggle", "dropdown")
	    .append(' <b class="caret"></b>');

	dropdown.find('> ul').addClass("dropdown-menu");

	/* Add divider class if menu item is empty */
	dropdown.parent().find('.dropdown-menu li').each(function() {
	    if ( $(this).text() == '\n') $(this).addClass('divider')
	});

	/* Compute page min height */
	$('div#page').css('min-height', $(window).innerHeight() -
			  $('#footer').outerHeight() -
			  $('div.navbar-fixed-top.navbar').outerHeight() -
			  parseInt($('div#page').css('padding-top')) -
			  parseInt($('div#page').css('padding-bottom')));


	/*
	 * Org exports html starting from h3.
	 * This promotes all nodes 2 steps up.
	 */
	var article_content = $('#page.container div.article-content');
	for (var i=1; i<=6; i++) {
	    article_content.find('h'+(i+2)).each(function() {
		$(this).replaceWith($('<h'+i+'/>').html($(this).html()));
            });
	    /* Do not forget outline classes */
	    article_content.find('.outline-' + (i+2))
		.removeClass('outline-' + (i+2))
		.addClass('outline-'+i);

	    article_content.find('.outline-text-' + (i+2))
		.removeClass('outline-text-' + (i+2))
		.addClass('outline-text-'+i);

	}


	$("i").each(function() {
	    if (/^icon-/.exec($(this).text())) {
		$(this).replaceWith('<i class="' + $(this).text() + '"></i>&nbsp;');
	    }
	});



    }
)
