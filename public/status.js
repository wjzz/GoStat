function checkStatus(sampleSize, timeSample, totalSize) {   
    $("#progressbar").progressbar({
            value: 0
		}); 

    updateStatus(sampleSize, timeSample, totalSize);
 }

function updateStatus(sampleSize, timeSample, totalSize){
    $.get("/status", function(data){
            if (!(data == "free")){
                var n         = parseInt(data);
                var totalTime = (timeSample * totalSize) / sampleSize;
                var timeLeft  = parseInt (((100 - n) * totalTime) / (100 * 1000));

                $("#progressbar").progressbar( "option", "value", n);
                $("#percent").html(data + "%");
                $("#timeLeft").html(timeLeft + " s");

                if (n < 100){
                    updateStatus(sampleSize, timeSample, totalSize);
                } else {
                    window.location.href = "/";
                }
            } else {
                $("#progressbar").progressbar( "option", "value", 100);
                window.location.href = "/";
            }
        });  
}

