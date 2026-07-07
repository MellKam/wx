// A hand-trimmed stand-in for `monaco-editor`'s own `editor.main.js` entry
// point. `editor.main.js` (what plain `import "monaco-editor"` resolves to)
// pulls in every editor contrib feature (hover, suggest, rename, format,
// semantic tokens, bracket matching, ...) *and* ~100 `basic-languages`
// Monarch definitions *and* the four built-in "rich" language services
// (css/html/json/typescript) — css/html/json alone are what drag in
// `css.worker.js`, `html.worker.js`, and `json.worker.js`, none of which
// this playground ever uses (wx registers its own language and its own
// worker-less LSP bridge).
//
// This file is copied from `editor.main.js`'s import list verbatim, minus
// the css/html/json language-service imports and every `basic-languages/*`
// import — every editor contrib feature is kept, since dropping one of
// those (e.g. semantic tokens, or marker/diagnostics decorations) would be a
// silent feature regression rather than a size win. The paths below are
// `monaco-editor`'s internal module layout, not public API, so they're only
// as stable as the pinned `monaco-editor` version in `package.json`.
//
// The typescript language service *is* kept (unlike the other three) so
// `host.js` files get real JS hover/completion/diagnostics — Monaco has no
// JS-only variant; `monaco.contribution.js` registers both "typescript" and
// "javascript" off the same `ts.worker.js` (~6.7MB), an accepted cost since
// nothing else here needs `.ts` file support. Like `wabt`, the worker is
// only fetched once a JS/TS model actually needs it, not at page load.
//
// `language/typescript/monaco.contribution.js` only wires up the *semantic*
// service (diagnostics/completion/hover) — it registers the "javascript"
// language id but never calls `setMonarchTokensProvider` for it, so without
// a tokenizer a `.js` model renders as unstyled plain text despite hover
// and completions working. That tokenizer normally comes from
// `basic-languages/javascript/javascript.js`, which is why (unlike every
// other basic-language) it's kept here — it's the one JS actually needs.
import "monaco-editor/esm/vs/basic-languages/javascript/javascript.contribution.js";
import "monaco-editor/esm/vs/language/typescript/monaco.contribution.js";
import "monaco-editor/esm/vs/editor/browser/coreCommands.js";
import "monaco-editor/esm/vs/editor/browser/widget/codeEditor/codeEditorWidget.js";
import "monaco-editor/esm/vs/editor/browser/widget/diffEditor/diffEditor.contribution.js";
import "monaco-editor/esm/vs/editor/contrib/anchorSelect/browser/anchorSelect.js";
import "monaco-editor/esm/vs/editor/contrib/bracketMatching/browser/bracketMatching.js";
import "monaco-editor/esm/vs/editor/contrib/caretOperations/browser/caretOperations.js";
import "monaco-editor/esm/vs/editor/contrib/caretOperations/browser/transpose.js";
import "monaco-editor/esm/vs/editor/contrib/clipboard/browser/clipboard.js";
import "monaco-editor/esm/vs/editor/contrib/codeAction/browser/codeActionContributions.js";
import "monaco-editor/esm/vs/editor/contrib/codelens/browser/codelensController.js";
import "monaco-editor/esm/vs/editor/contrib/colorPicker/browser/colorPickerContribution.js";
import "monaco-editor/esm/vs/editor/contrib/comment/browser/comment.js";
import "monaco-editor/esm/vs/editor/contrib/contextmenu/browser/contextmenu.js";
import "monaco-editor/esm/vs/editor/contrib/cursorUndo/browser/cursorUndo.js";
import "monaco-editor/esm/vs/editor/contrib/dnd/browser/dnd.js";
import "monaco-editor/esm/vs/editor/contrib/dropOrPasteInto/browser/copyPasteContribution.js";
import "monaco-editor/esm/vs/editor/contrib/dropOrPasteInto/browser/dropIntoEditorContribution.js";
import "monaco-editor/esm/vs/editor/contrib/find/browser/findController.js";
import "monaco-editor/esm/vs/editor/contrib/folding/browser/folding.js";
import "monaco-editor/esm/vs/editor/contrib/fontZoom/browser/fontZoom.js";
import "monaco-editor/esm/vs/editor/contrib/format/browser/formatActions.js";
import "monaco-editor/esm/vs/editor/contrib/documentSymbols/browser/documentSymbols.js";
import "monaco-editor/esm/vs/editor/contrib/inlineCompletions/browser/inlineCompletions.contribution.js";
import "monaco-editor/esm/vs/editor/contrib/inlineProgress/browser/inlineProgress.js";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/goToCommands.js";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/link/goToDefinitionAtPosition.js";
import "monaco-editor/esm/vs/editor/contrib/gotoError/browser/gotoError.js";
import "monaco-editor/esm/vs/editor/contrib/gpu/browser/gpuActions.js";
import "monaco-editor/esm/vs/editor/contrib/hover/browser/hoverContribution.js";
import "monaco-editor/esm/vs/editor/contrib/indentation/browser/indentation.js";
import "monaco-editor/esm/vs/editor/contrib/inlayHints/browser/inlayHintsContribution.js";
import "monaco-editor/esm/vs/editor/contrib/inPlaceReplace/browser/inPlaceReplace.js";
import "monaco-editor/esm/vs/editor/contrib/insertFinalNewLine/browser/insertFinalNewLine.js";
import "monaco-editor/esm/vs/editor/contrib/lineSelection/browser/lineSelection.js";
import "monaco-editor/esm/vs/editor/contrib/linesOperations/browser/linesOperations.js";
import "monaco-editor/esm/vs/editor/contrib/linkedEditing/browser/linkedEditing.js";
import "monaco-editor/esm/vs/editor/contrib/links/browser/links.js";
import "monaco-editor/esm/vs/editor/contrib/longLinesHelper/browser/longLinesHelper.js";
import "monaco-editor/esm/vs/editor/contrib/middleScroll/browser/middleScroll.contribution.js";
import "monaco-editor/esm/vs/editor/contrib/multicursor/browser/multicursor.js";
import "monaco-editor/esm/vs/editor/contrib/parameterHints/browser/parameterHints.js";
import "monaco-editor/esm/vs/editor/contrib/placeholderText/browser/placeholderText.contribution.js";
import "monaco-editor/esm/vs/editor/contrib/rename/browser/rename.js";
import "monaco-editor/esm/vs/editor/contrib/sectionHeaders/browser/sectionHeaders.js";
import "monaco-editor/esm/vs/editor/contrib/semanticTokens/browser/documentSemanticTokens.js";
import "monaco-editor/esm/vs/editor/contrib/semanticTokens/browser/viewportSemanticTokens.js";
import "monaco-editor/esm/vs/editor/contrib/smartSelect/browser/smartSelect.js";
import "monaco-editor/esm/vs/editor/contrib/snippet/browser/snippetController2.js";
import "monaco-editor/esm/vs/editor/contrib/stickyScroll/browser/stickyScrollContribution.js";
import "monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestController.js";
import "monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestInlineCompletions.js";
import "monaco-editor/esm/vs/editor/contrib/tokenization/browser/tokenization.js";
import "monaco-editor/esm/vs/editor/contrib/toggleTabFocusMode/browser/toggleTabFocusMode.js";
import "monaco-editor/esm/vs/editor/contrib/unicodeHighlighter/browser/unicodeHighlighter.js";
import "monaco-editor/esm/vs/editor/contrib/unusualLineTerminators/browser/unusualLineTerminators.js";
import "monaco-editor/esm/vs/editor/contrib/wordHighlighter/browser/wordHighlighter.js";
import "monaco-editor/esm/vs/editor/contrib/wordOperations/browser/wordOperations.js";
import "monaco-editor/esm/vs/editor/contrib/wordPartOperations/browser/wordPartOperations.js";
import "monaco-editor/esm/vs/editor/contrib/readOnlyMessage/browser/contribution.js";
import "monaco-editor/esm/vs/editor/contrib/diffEditorBreadcrumbs/browser/contribution.js";
import "monaco-editor/esm/vs/editor/contrib/floatingMenu/browser/floatingMenu.contribution.js";
import "monaco-editor/esm/vs/editor/common/standaloneStrings.js";
import "monaco-editor/esm/vs/base/browser/ui/codicons/codicon/codicon.css";
import "monaco-editor/esm/vs/base/browser/ui/codicons/codicon/codicon-modifiers.css";
import "monaco-editor/esm/vs/editor/standalone/browser/iPadShowKeyboard/iPadShowKeyboard.js";
import "monaco-editor/esm/vs/editor/standalone/browser/inspectTokens/inspectTokens.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneHelpQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoLineQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoSymbolQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneCommandsQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/referenceSearch/standaloneReferenceSearch.js";
import "monaco-editor/esm/vs/editor/standalone/browser/toggleHighContrast/toggleHighContrast.js";

export * from "monaco-editor/esm/vs/editor/editor.api.js";
