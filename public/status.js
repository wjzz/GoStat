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
                    
                    if (n < 100){
                        updateStatus();
                    }
            } else {
                $("#progressbar").progressbar( "option", "value", 100);                    
            }
        });  
}

/*
            for (i=0; i <= 100; ++i){
                alert("hello" + i);
                $("#progressbar").progressbar( "option", "value", i);
                }
*/