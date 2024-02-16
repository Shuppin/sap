import init, {interpret, parse, lex} from "./playground.js";

let wasmModulePromise = init().then(wasmModule => {
    return wasmModule;
});

var editor = CodeMirror.fromTextArea(document.getElementById('codeeditor'), {
    lineNumbers: true,
    mode: "the only way to disable syntax highlighting is to put some non-existant language into this argument",
    theme: "default",
    indentUnit: 4,
    indentWithTabs: true
});

editor.setSize("100%", "100%")

// Select the button using its unique ID
const button = document.getElementById('run-button');

// Add an event listener for the 'click' event
button.addEventListener('click', function() {
    wasmModulePromise.then(() => {
        // Retrieve the selected value from the <select> element when the  button is clicked
        const selectedOutputMode = document.getElementById('output-modes').value;
        var resultOutput = document.getElementById("result-content");
        var stdoutOutput = document.getElementById("stdout-content");
        resultOutput.textContent = ""
        stdoutOutput.textContent = ""
        var source = editor.getValue();
        switch (selectedOutputMode) {
            case 'eval':
                interpret(false, source);
                break;
            case 'env':
                interpret(true, source);
                break;
            case 'ast':
                parse(source);
                break;
            case 'tokens':
                lex(source);
                break;
            default:
                console.log("Unexpected output mode:", selectedOutputMode)

        }
    });

});
