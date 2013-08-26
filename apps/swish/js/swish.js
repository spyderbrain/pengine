var env = {};

env.prolog = null;
env.dirty = false;
env.history = [];
env.maxHistoryLength = 15;

env.editor = ace.edit("editor");
env.editor.setTheme("ace/theme/spyder");
env.editor.getSession().setMode("ace/mode/prolog");
env.editor.setHighlightActiveLine(false);
env.editor.renderer.setShowPrintMargin(false);
env.editor.session.setFoldStyle("manual");

env.cmdline = ace.edit("cmdlineeditor");
env.cmdline.setTheme("ace/theme/spyder");
env.cmdline.getSession().setMode("ace/mode/prolog");
env.cmdline.setHighlightActiveLine(false);
env.cmdline.renderer.setShowPrintMargin(false);
env.cmdline.renderer.setShowGutter(false);
env.cmdline.getSession().setUseWrapMode(true);
env.cmdline.session.setWrapLimitRange(null, null);

env.prolog = new Pengine({
    onload: handleLoaded,
    onanswer: handleAnswer,
    onhalted: handleHalted,
    onprompt: handlePrompt,
    onoutput: handleOutput,
    onerror: handleError
});


// Calling Prolog

function consult() {
    var program = getProgram().trim();
    env.prolog.load(program);   
}

function ask() {  
    var query = getGoal()
    query = query.replace(/^\?-/, '');
    query = query.trim();
    if (query) {
    	addmsg(renderQuery(query  + "."), "goal");
    	updateHistory(query);
    	env.prolog.query(query, 'json-s');
    }
}

function more() {
    addmsg(" ;<br />", "solution");
    env.prolog.next();
}

function stop() {
    addmsg(".<br />", "solution");
    env.prolog.stop();
}

function abort() {
    env.prolog.abort();
}

function clear() {
    $("#presentation").html("");
    addmsg("?- ", "goal");
}

function read() {
    var reader = $("#reader");
    var str = reader.val();
    if (str) {
        env.prolog.input(str);
        $('#prompt').html("|:");
        reader.val("");
        reader.prop("disabled", true);
        reader.css("background-color", "white");
        disableButtons(false, true, true, true);
    }
}
    
// Handling Prolog callbacks

function handleLoaded() {
    ask();
}

function handleAnswer() {
    var success = this.data.success;
    var html;
    if (success) {
	    var bindings = this.data.bindings;
	    if (!jQuery.isEmptyObject(bindings)) {
	        html = renderBindings(bindings)
	    } else {
	        html = "<span class='true'>true</span>";
	    }
	    if (this.data.more) {
	    	addmsg(html, "solution");
			disableButtons(true, false, false, false);
		} else {
	    	addmsg(html + ".<br />", "solution");
			disableButtons(false, true, true, false);   	
			addmsg("?- ", "goal"); 		
		}   	
    } else {
		addmsg("false.<br />", "solution false")
    	disableButtons(false, true, true, false);   	
		addmsg("?- ", "goal");
	}
}
function handleHalted() {
    addmsg("?- ", "goal");
    disableButtons(false, true, true, false);
}
function handlePrompt() {
    var reader = $("#reader");
    reader.prop("disabled", false);
    reader.focus();
    $("#reader").placeholder = this.data;
    $("#reader").css('background-color', 'rgb(252, 248, 227)');
    disableButtons(true, true, true, false);
}
function handleOutput() {
	var data = this.data.trim();
	data = data.replace(/^'/, "");
	data = data.replace(/'$/, "");
	data = data.replace(/'<br \/>$/, "<br \/>");
    addmsg(data, "output");
    disableButtons(true, true, true, false);
}
function handleError() {
    var html = highlight2("% " + this.data) + "<br />";
    addmsg(html, "solution");
    addmsg("?- ", "goal");
    disableButtons(false, true, true, false);
}



// Getting and setting program and goal

function getProgram() {
    return env.editor.getValue()
}

function setProgram(src) {
	env.editor.setValue(src, -1);
}

function getGoal() {
    var val = env.cmdline.getValue();
    val = val.trim();
    if (val.charAt(val.length-1) === ".") {
        return val.slice(0, -1);
    } else {
        return val;
    }
}

function setGoal(Query) {
    env.cmdline.setValue(Query, 1000)
    env.cmdline.focus()
}



// Presentation

function addmsg(msg, style) {
    $("#presentation").append("<span class='" + theme() + " " + style + "'>" + msg + "</span>");
    $("#results").scrollTop($("#results").prop('scrollHeight'));
}

function highlight(string) {
    var html = [];
    var data = env.editor.session.getMode().getTokenizer().getLineTokens(string, "#statement"); 
    env.editor.renderer.$textLayer.$renderSimpleLine(html, data.tokens);
    return html.join("") 
}

function highlight2(string) {
    var html = [];
    var data = env.editor.session.getMode().getTokenizer().getLineTokens(string, "start"); 
    env.editor.renderer.$textLayer.$renderSimpleLine(html, data.tokens);
    return html.join("") 
}

function theme() {
    return env.editor.renderer.theme.cssClass;
}

function renderQuery(query) {
	return highlight(query) + "<br />";
}

function renderBindings(bindings) {
    var html = "";
    for (var i in bindings) {
       if (typeof bindings[i] == "object") {
           html += highlight(i + ' = [' + bindings[i] + ']') + ",<br />";;
       } else {
           html += highlight(i + ' = ' + bindings[i]) + ",<br />";;
       }
    }
    html = html.replace(/,<br \/>$/, "");
    return html;
}

function disableButtons(ask, more, stop, abort) {
    $("#ask-btn").prop("disabled", ask);
    $("#more-btn").prop("disabled", more);
    $("#stop-btn").prop("disabled", stop);
    $("#abort-btn").prop("disabled", abort);
}


// Printing

function print_editor_content() {
	var iframe = document.createElement("iframe");
	iframe.style.display = "none"
	document.body.appendChild(iframe)
	var windw = iframe.contentWindow;
	windw.document.open();
    windw.document.write('</head><body><pre>');
    windw.document.write(getProgram());
    windw.document.write('</pre></body></html>');
    windw.print();
    windw.document.close();
    document.body.removeChild(iframe);
}	
	    

// GUI preferences

function setTheme(theme) {
	env.editor.setTheme("ace/theme/" + theme);
	env.cmdline.setTheme("ace/theme/" + theme);
	$("#theme-menu option:selected").prop("selected", false);
	$("#theme-menu").find("option[value='" + theme +"']").prop("selected", true);
}

function setFontFamily(family) {
	$('#editor, #cmdlineeditor, #presentation, #examples, #history').css('fontFamily', family);
	$("#font-family-menu option:selected").prop("selected", false);
	$("#font-family-menu").find("option[value='" + family +"']").prop("selected", true);	
}

function setFontSize(size) {
	$('#editor, #cmdlineeditor, #presentation, #examples, #history').css('fontSize', size + 'px');
	$("#font-size-menu option:selected").prop("selected", false);
	$("#font-size-menu").find("option[value=" + size +"]").prop("selected", true);	
}

function setTabSize(n) {
	env.editor.getSession().setTabSize(n);
	$("#tab-size-menu option:selected").prop("selected", false);
	$("#tab-size-menu").find("option[value=" + n +"]").prop("selected", true);	
}

function setUseSoftTabs(bool) {
	env.editor.getSession().setUseSoftTabs(bool);
	$("#tab-soft-checkbox").prop('checked', bool);
}

function setLineWrap(bool) {
	env.editor.getSession().setUseWrapMode(bool);
	$("#line-wrap-checkbox").prop('checked', bool);
}

function setLineHighlight(bool) {
	env.editor.setHighlightActiveLine(bool);
	$("#line-highlight-checkbox").prop('checked', bool);
}

function setShowGutter(bool) {
	env.editor.renderer.setShowGutter(bool);
	$("#line-numbering-checkbox").prop('checked', bool);
}


// Handling programs
    
function maybeLoadSrc() {
    var file = window.location.hash.slice(1);
    if (file) {
        loadSrc("/storage/"+ encodeURIComponent(file));            
    }
}

function loadSrc(url) {
    $.get(url)
    .done(function(program) {
    		setProgram(program);
			var examples = extractExamples()[0];
			if (examples) {
				setGoal("?- " + examples[0]);
			}
			populateExampleMenu();
			env.dirty = false;
			$('#save-btn').prop('disabled', true)
	})
	.fail(function() {
		$("#presentation").html('<div class="alert alert-error"> <button type="button" class="close" data-dismiss="alert">&times;</button> Error: ' + url + ' does not exist.</div>');
		addmsg("?- ", "goal");
	}) 
}

function saveProgram() {
    var program = encodeURIComponent(getProgram());
    if (program) {
        $.post("/storage/store", "program=" + program, function(response) {
            var url = response.url;
            var file = response.file;
            window.location.hash = file;
            $("#url").val(url + "/apps/swish/index.html#" + file);
            $('#save-btn').addClass("hide");
            $('#update-btn').removeClass("hide").prop('disabled', true);
            $('#share-btn').removeClass("hide");
            env.dirty = false;
        });
    }
}

function updateProgram() {
	var file = window.location.hash.slice(1);
    var program = encodeURIComponent(getProgram());
    if (program) {
         $.post("/storage/update", "file=" + file + "&program=" + program, function() {
			$('#update-btn').prop('disabled', true);
            env.dirty = false;
        });
    }
}


function extractExamples() {
    var Search = ace.require("./search").Search;
    var search = new Search();
    search.setOptions({
        needle: /\/\*\*\s*Examples/,
        range: null,
        caseSensitive: false,
        regExp: true
    });
    var ranges = search.findAll(env.editor.session)
    var doc = env.editor.session.getDocument();
    var examples = []
    for (var i in ranges) {
        var examplegroup = [];
        var row = ranges[i].start.row;
        for (var j = row + 1; ; j++) {
            var ex = doc.getLine(j).trim();
            if (ex == "*/") {
                break;
            } else {
                if (ex != "") {
                    examplegroup.push(ex);
                }
            }
        }
        examples.push(examplegroup);
    }
    return examples;
}


function examplesToHTML(examples) {
    var html = [];
    for (var i in examples) {
        var examplegroup = examples[i];
        for (var j in examplegroup) {
            var ex = examplegroup[j];
            ex = "<li><a href='#' onclick='setGoal(\"?- " + ex + "\")'>?- " + ex + "</a></li>";
            html.push(ex);
        }
        html.push("<li class='divider'></li>")
    }
    html.pop(); // get rid of the last divider
    return html.join("");
}


function populateExampleMenu() {
    var html = examplesToHTML(extractExamples());
    $("#examples").html(html);
}


function updateHistory(query) {
	var history = env.history;
	var index = history.indexOf(query);
	if (index != -1) history.splice(index, 1);
	if (history.length >= env.maxHistoryLength) history.shift();
	env.history.push(query);
}

function populateHistoryMenu() {
	var html = "";
	var history = env.history;
	for (var i in history) {
		html += "<li><a href='#' onclick='setGoal(\"?- " + history[i] + ".\")'>?- " + history[i] + ".</a></li>";
	}
    $("#history").html(html);}




// Event handlers: Editor

env.editor.getSession().on('change', function() {
	if (!env.dirty) { 
    	env.dirty = true;
		$('#save-btn').prop('disabled', false);
		$('#update-btn').prop('disabled', false);
	}
});


// Event handlers: Menus

$("#example-menu").on("click", "a", function(evt) {
	evt.preventDefault();
	loadSrc(evt.target.href);
});

$("#file-menu").on("click", "a#prefs", function(evt) {
	evt.preventDefault();
	$("#preferences").modal({backdrop:false});
});

$("#file-menu").on("click", "a#print", function(evt) {
	evt.preventDefault();
	print_editor_content();
});

$("#edit-menu").on("click", "a#undo", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.undo.exec(env.editor)
});

$("#edit-menu").on("click", "a#redo", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.redo.exec(env.editor)
});

$("#edit-menu").on("click", "a#indent", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.indent.exec(env.editor)
});

$("#edit-menu").on("click", "a#outdent", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.outdent.exec(env.editor)
});

$("#edit-menu").on("click", "a#comment", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.toggleBlockComment.exec(env.editor)
});

$("#edit-menu").on("click", "a#find", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.replace.exec(env.editor, "left")
});


// Event handlers: Prefeences

$("#theme-menu").on("change", function() {
	var value = $("#theme-menu option:selected").val();
	setTheme(value);
	if (localStorage) {
		localStorage['swish-theme'] = value;
	}
});

$("#font-family-menu").on("change", function() {
	var value = $("#font-family-menu option:selected").val();
	setFontFamily(value);
	if (localStorage) {
		localStorage['swish-font-family'] = value;
	}
});

$("#font-size-menu").on("change", function() {
	var value = $("#font-size-menu option:selected").val();
	setFontSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['swish-font-size'] = value;
	}
});

$("#tab-size-menu").on("change", function() {
	var value = $("#tab-size-menu option:selected").val();
	setTabSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['swish-tab-size'] = value;
	}
});

$("#tab-soft-checkbox").on("change", function() {
	var value = $("#tab-soft-checkbox").prop('checked');
	setUseSoftTabs(value);
	if (localStorage) {
		localStorage['swish-tab-soft'] = value;
	}
});

$("#line-wrap-checkbox").on("change", function() {
	var value = $("#line-wrap-checkbox").prop('checked');
	setLineWrap(value);
	if (localStorage) {
		localStorage['swish-line-wrap'] = value;
	}
});

$("#line-highlight-checkbox").on("change", function() {
	var value = $("#line-highlight-checkbox").prop('checked');
	setLineHighlight(value);
	if (localStorage) {
		localStorage['swish-line-highlight'] = value;
	}
});

$("#line-numbering-checkbox").on("change", function() {
	var value = $("#line-numbering-checkbox").prop('checked');
	setShowGutter(value);
	if (localStorage) {
		localStorage['swish-line-numbering'] = value;
	}
});

$("#save-btn").on("click", saveProgram); 

$("#update-btn").on("click", updateProgram); 

$("#share-btn").on("click", function() {
    updateProgram();
    $('#share').modal();
}); 


// Event handlers: Console
 
$("#examples-btn").on("click", function() {
	if (env.dirty) {
		populateExampleMenu();
	}
}); 

$("#history-btn").on("click", populateHistoryMenu);

$("#clear-btn-query").on("click", function() {
	setGoal("?- ")
}); 
 
//$("#ask-btn").on("click", ask);
$("#ask-btn").on("click", consult);
$("#more-btn").on("click", more);
$("#stop-btn").on("click", stop);
$("#abort-btn").on("click", abort);
$("#clear-btn").on("click", clear);

$("#reader").on("keyup", function(evt) {
	if (evt.keyCode == 13) {
		read();
	}
});
$("#reader").on("blur", function(evt) {
	evt.target.focus(); 
	return false;
});

function parseBoolean(value) {
	return value == "true" ? true : false;
}

$(document).ready(function() {
	if (localStorage && localStorage.length > 0) {
		setTheme(localStorage['swish-theme']);
		setFontFamily(localStorage['swish-font-family']);
		setFontSize(localStorage['swish-font-size']);
		setTabSize(parseInt(localStorage['swish-tab-size'], 10));
		setLineWrap(parseBoolean(localStorage['swish-line-wrap']));
		setLineHighlight(parseBoolean(localStorage['swish-line-highlight']));
		setShowGutter(parseBoolean(localStorage['swish-line-numbering']));
		setUseSoftTabs(parseBoolean(localStorage['swish-tab-soft']));
	}
    maybeLoadSrc();
    addmsg("?- ", "goal");
	setGoal("?- ");
});

