var watbrowser = (function() {

    function init() {
	wat_env = wat.mkenvcore();
        var forms = [];
        // start = new Date().getTime();
        // elapsed = new Date().getTime() - start;
        // console_log("Evaluation time " + elapsed + "ms");
	forms = forms.concat(load_file("../crust.wat"));
	forms = forms.concat(load_file("../test.wat"));
	forms = forms.concat(load_file("browser.wat"));
	forms = forms.concat(load_file("../repl.wat"));
        wat.eval(wat.array_to_list([new wat.Sym("begin")].concat(forms)), wat_env);
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
            return forms;
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
