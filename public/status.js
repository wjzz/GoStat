function checkStatus() {   
    $("#progressbar").progressbar({
            value: 0
		}); 

    updateStatus();
 }

function updateStatus(){
    $.get("/status", function(data){
            if (!(data == "free")){
                    var n = parseInt(data);
                    $("#progressbar").progressbar( "option", "value", n);
                    $("#percent").html("<h1>" + data + "%</h1>");
                    
                    if (n < 100){
                        updateStatus();
                    } else {
                        window.location.href = "/";
                    }
            } else {
                $("#progressbar").progressbar( "option", "value", 100);
                window.location.href = "/";
            }
        });  
}
