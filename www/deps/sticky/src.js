$(function() {

    var $sidebar   = $("#sidebar"), 
        $window    = $(window),
        offset     = $sidebar.offset(),
        leftPadding = 0;

    $window.scroll(function() {
        if ($window.scrollLeft() > offset.left) {
            $sidebar.stop().animate({
                marginLeft: $window.scrollLeft() - offset.left + leftPadding
            });
        } else {
            $sidebar.stop().animate({
                marginLeft: 0
            });
        }
    });
}); 





/*


$(function() {

    var offset = $("#sidebar").offset();
    var topPadding = 15;

    $(window).scroll(function() {
    
        if ($(window).scrollTop() > offset.top) {
        
            $("#sidebar").stop().animate({
            
                marginTop: $(window).scrollTop() - offset.top + topPadding
            
            });
        
        } else {
        
            $("#sidebar").stop().animate({
            
                marginTop: 0
            
            });
        
        }
        
            
    });

});*/

