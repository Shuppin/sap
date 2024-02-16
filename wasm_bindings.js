function appendTextToResultOutput(text) {
    var output = document.getElementById('result-content');
    output.textContent += text;
}

function appendTextToStandardOutput(text) {
    var output = document.getElementById('stdout-content');
    output.textContent += text;
}