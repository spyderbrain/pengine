var boardsize = 8;
var boardwidth = 400;
var pengine = new Pengine({
    onanswer:handleAnswer,
    onerror:handleError
});
function board(size, width) {
    var board = "";
    var fieldsize = Math.floor(width/size);
    for(var i=1; i<=size; i++) {
        board += row(i, size, fieldsize);
    }
    $("#board").html(board);
}
function row(n, size, fieldsize) {
    var y = fieldsize * (n-1);
    var row = "<div class='row' style='top:"+y+"px; left:0px;'>\n";
    for ( var i=1; i<=size; i++ ) {
        var id = i+"-"+n;
        var oe = (i+n)%2 == 0 ? "even" : "odd";
        row += "<div id='"+id+"' class='square "+oe +
           "' style='width:"+fieldsize+"px; height:"+fieldsize+"px'></div>\n";
    }
    row += "</div>\n";
    return row;
}
function clearBoard() {
    for (var i = 1; i <= boardsize; i++) {
        for (var j = 1; j <= boardsize; j++) {
            $("#"+i+"-"+j).html("");
        }
    }
}
function setQueens(squareList) {
    for (var i = 1; i <= boardsize; i++) {
        var id = i+"-"+squareList[i-1];
        $("#"+id).html("<img src='img/queen.png' class='square-img'/>");
    }
}
function handleAnswer() {
    if (this.data.success) {
        clearBoard();
        setQueens(this.data.bindings.Queens);
    } else {
        $("#msg").html("There are no more solutions.");
    }
}
function handleError() {
    $("#msg").html(this.data);
}
function query() {
    $("#msg").html("");
    pengine.query("queens(8, Queens)");
}
function next() {
    pengine.next();            
}
