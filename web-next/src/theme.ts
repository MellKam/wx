import type * as monaco from "./monaco-lite";
import darkVs from "@codingame/monaco-vscode-theme-defaults-default-extension/resources/dark_vs.json";
import darkPlus from "@codingame/monaco-vscode-theme-defaults-default-extension/resources/dark_plus.json";
import darkModern from "@codingame/monaco-vscode-theme-defaults-default-extension/resources/dark_modern.json";

export const WX_DARK_THEME = "wx-dark-modern";

interface VscodeTokenColor {
	scope?: string | string[];
	settings: { foreground?: string; background?: string; fontStyle?: string };
}

function toRules(tokenColors: VscodeTokenColor[]): monaco.editor.ITokenThemeRule[] {
	const rules: monaco.editor.ITokenThemeRule[] = [];
	for (const { scope, settings } of tokenColors) {
		const scopes = scope === undefined ? [""] : Array.isArray(scope) ? scope : [scope];
		for (const token of scopes) {
			rules.push({
				token,
				foreground: settings.foreground?.replace(/^#/, ""),
				background: settings.background?.replace(/^#/, ""),
				fontStyle: settings.fontStyle,
			});
		}
	}
	return rules;
}

/**
 * Colors for wx-lsp's LSP semantic token types (see `SEMANTIC_TOKEN_TYPES` in
 * `wx-lsp/src/lib.rs`), keyed directly by the LSP type name (`"struct"`,
 * `"enumMember"`, ...) rather than a TextMate scope. Monaco's semantic-token
 * styling resolves each token by trying the literal LSP type/modifier name
 * as a candidate right alongside its usual TextMate-scope translation, so a
 * `rules` entry whose `token` is the bare type name is matched directly —
 * the same technique used by microsoft/monaco-editor's own
 * "semantic-tokens-provider-example" playground sample.
 *
 * This exists because `dark_plus.json`'s own `tokenColors` collapse
 * `struct`/`enum`/`interface`/`type`/`namespace` into one shared
 * "entity.name.type"-ish teal — that's how plain TextMate/Monarch
 * highlighting is *supposed* to look without semantic info, but it's flatter
 * than what a real LSP with semantic tokens should render, so these rules
 * give each of wx-lsp's semantic types its own color instead of inheriting
 * that grouping wholesale.
 */
const SEMANTIC_TOKEN_RULES: monaco.editor.ITokenThemeRule[] = [
	{ token: "function", foreground: "DCDCAA" },
	{ token: "variable", foreground: "9CDCFE" },
	{ token: "parameter", foreground: "9CDCFE", fontStyle: "italic" },
	{ token: "namespace", foreground: "4EC9B0" },
	{ token: "type", foreground: "4EC9B0" },
	{ token: "struct", foreground: "86C691" },
	{ token: "interface", foreground: "B8D7A3" },
	{ token: "enum", foreground: "4EC9B0" },
	{ token: "enumMember", foreground: "4FC1FF" },
	{ token: "typeParameter", foreground: "4EC9B0", fontStyle: "italic" },
];

/**
 * VS Code's real "Dark Modern" default theme, resolved by hand for classic
 * (non-workbench) Monaco. `dark_modern.json` only carries `colors` — it
 * `include`s `dark_plus.json` for syntax `tokenColors`, which in turn
 * `include`s `dark_vs.json`. VS Code's include resolution concatenates
 * `tokenColors` base-first (so the child's entries, being later, win on
 * conflicting scopes) and lets each theme's own `colors` win outright — so
 * that's reproduced manually here since plain `monaco-editor` has no VS
 * Code theme/textmate service to do this resolution for us; only
 * `monaco.editor.defineTheme`'s own flat `rules`/`colors` shape exists.
 *
 * `SEMANTIC_TOKEN_RULES` is appended last so it wins over the inherited
 * dark_plus/dark_vs entries for the same scopes.
 */
export function buildWxDarkThemeData(): monaco.editor.IStandaloneThemeData {
	return {
		base: "vs-dark",
		inherit: false,
		rules: [
			...toRules(darkVs.tokenColors),
			...toRules(darkPlus.tokenColors),
			...SEMANTIC_TOKEN_RULES,
		],
		colors: darkModern.colors,
	};
}
