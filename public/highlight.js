function lstMouseOver(img, move){
    //$("#brd"+move).css("color", "red");
    $("#brd"+move).html("<img src=\"" + img + "\" />");
}
function lstMouseOut(move){
    //$("#brd"+move).css("color", "blue");    
    $("#brd"+move).html("x");

}