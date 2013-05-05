var watbrowser = (function() {
    
    var wat = Wat();

    function init() {
        var forms = [];
	forms.push(load_file("../prim.wat"));
	forms.push(load_file("../test.wat"));
        start = new Date().getTime();
        wat.eval(wat.array_to_list([new wat.Sym("wat-begin")].concat(forms)));
        elapsed = new Date().getTime() - start;
        console_log("Total evaluation time " + elapsed + "ms");
    }

    function load_file(path) {
	console_log("Loading " + path + "...");
	var req = new XMLHttpRequest();
	// Append random thang to file path to bypass browser cache.
	req.open("GET", path + "?" + Math.random(), false);
	req.send(null);
	if(req.status == 200) {
            var start = new Date().getTime();
            var form = wat.parse(req.responseText);
            var elapsed = new Date().getTime() - start;
            console_log("Parse time " + elapsed + "ms");
            return form;
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
