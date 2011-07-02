function rebuildConfirm(msg, url){
    var answer = confirm(msg);
    if (answer){
        window.location.href = url;
    }
}