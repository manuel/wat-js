var watbrowser = (function() {

    function init() {
	wat_env = wat.mkenvcore();
	load_file("../crust.wat");
	load_file("../test.wat");
	load_file("browser.wat");
	load_file("../repl.wat");
    }

    function load_file(path) {
	console_log("Loading " + path + "...");
	var req = new XMLHttpRequest();
	// Append random thang to file path to bypass browser cache.
	req.open("GET", path + "?" + Math.random(), false);
	req.send(null);
	if(req.status == 200) {
	    //        try {
            var start = new Date().getTime();
            var forms = wat.parse(req.responseText);
            var elapsed = new Date().getTime() - start;
            console_log("Parse time " + elapsed + "ms");
            start = new Date().getTime();
            for (var i = 0; i < forms.length; i++) {
                var result = wat.eval(forms[i], wat_env);
//	        console_log(result);
            }
            elapsed = new Date().getTime() - start;
            console_log("Evaluation time " + elapsed + "ms");
	    //        } catch(e) {
	    //            wat_console_log("ERROR: " + e);
	    //        }
	} else {
            throw("XHR error: " + req.status);
	}
    }

    function console_log(string) {
	if (console) console.log(string);
    }

    return {
	"init": init,
    };

}());
