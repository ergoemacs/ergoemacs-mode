// Number lines





$(document).ready(
    function() {
	$(".modal .modal-body > pre").addClass('prettyprint').addClass('linenums');
	prettyPrint();
	$(".modal").css('width', '');
    }
);
