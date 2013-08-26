function Pengine(callbacks) {
    var that = this;
    // Private functions
    function process_response(obj) {
        if (obj.event === 'loaded') {
            if (callbacks.onload) callbacks.onload.call(obj);            
        } else if (obj.event === 'answer') {
            if (callbacks.onanswer) callbacks.onanswer.call(obj);
        } else if (obj.event === 'error') {
            if (callbacks.onerror) callbacks.onerror.call(obj);
        } else if (obj.event === 'output') {
            if (callbacks.onoutput) callbacks.onoutput.call(obj); 
            that.result();
        } else if (obj.event === 'prompt') {
            if (callbacks.onprompt) callbacks.onprompt.call(obj);
        } else if (obj.event === 'halted') {
            if (callbacks.onhalted) callbacks.onhalted.call(obj);
        } else if (obj.event === 'listed') {
            if (callbacks.onlist) callbacks.onlist.call(obj);
        } 
    };
    // Public functions
    this.load = function(program) {
        $.post('/prolog/consult', program, process_response);
    }  
    this.query = function(query, format) {
        var query = encodeURIComponent(query);
        if (typeof(format) === 'undefined') format = "json";
        $.get('/prolog/first?goal=' + query + '&format=' + format, process_response);
    }
    this.next = function() {
        $.get('/prolog/next', process_response);
    }
    this.stop = function() {
        $.get('/prolog/stop', process_response);
    }
    this.input = function(string) {
        var string = encodeURIComponent(string);
        $.get('/prolog/input?input=' + string, process_response);
    }
    this.abort = function() {
        $.get("/prolog/abort", process_response);
    }
    this.result = function() {
        $.get("/prolog/result", process_response);
    }
    this.listing = function() {
        $.get("/prolog/listing", process_response);
    }
    var scripts = document.getElementsByTagName('script');
    var source = "";
    for (var i = 0; i < scripts.length; i++) {
        if (scripts[i].getAttribute('type') == 'text/x-prolog') {
            source += '\n' + scripts[i].textContent;
        }
    }
    if (source.length > 0) {
        that.load(source);
    }
}
