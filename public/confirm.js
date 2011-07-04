function rebuildConfirm(msg, url){
    var answer = confirm(msg);
    if (answer){
        window.location.href = url;
    }
}

function checkDB(msg, ok_url, rebuild_url){
    $.get("/dbcheck", function(response){
            if (response == "True"){
                window.location.href = ok_url;
            } else if (response == "False"){
                rebuildConfirm(msg, rebuild_url);
            } else {
                alert("Impossible happened.");
            }
        });
}
