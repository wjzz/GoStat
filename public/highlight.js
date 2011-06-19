function lstMouseOver(img, move){
    $("#brd"+move).html("<img src=\"" + img + "\" />");
}
function lstMouseOut(move){
    $("#brd"+move).html("x");
}
function brdMouseOver(img, move){
    //lstMouseOver(img, move);
    $("#tr"+move + " > td").css("background-color", "lightblue");
}
function brdMouseOut(move){
    //lstMouseOut(move);
    $("#tr"+move + " > td").css("background-color", "white");
}