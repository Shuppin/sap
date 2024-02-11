import init, {interpret} from "./playground.js";

var editor = CodeMirror.fromTextArea(document.getElementById('codeEditor'), {
    lineNumbers: true,
    mode: "the only way to disable syntax highlighting is to put some non-existant language into this argument",
    theme: "default",
    indentUnit: 4,
    indentWithTabs: true
});

let wasmModulePromise = init().then(wasmModule => {
    return wasmModule;
});

window.run = function() {
    wasmModulePromise.then(() => {
        var output = document.getElementById("stdout");
        output.textContent = ""
        var source = editor.getValue();
        interpret(source);
    });
    // output.textContent = editor.getValue()
}