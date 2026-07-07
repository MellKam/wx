import { createApp } from "vue";
import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import TsWorker from "monaco-editor/esm/vs/language/typescript/ts.worker?worker";
import App from "./App.vue";
import "./style.css";

// No `MonacoVscodeApiWrapper`/extension host here — plain `monaco-editor`
// only asks for two workers: the generic editor worker (diffing etc.), and
// the typescript/javascript language-service worker for `.js` files (see
// monaco-lite.ts — json/css/html aren't registered, so they never ask).
// This has to run before the editor is ever created, so it lives here
// rather than inside `App.vue`'s `onMounted`.
self.MonacoEnvironment = {
	getWorker(_workerId, label) {
		if (label === "typescript" || label === "javascript") return new TsWorker();
		return new EditorWorker();
	},
};

createApp(App).mount("#app");
