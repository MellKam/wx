import type * as monaco from "./monaco-lite";

export const WX_LANGUAGE_ID = "wx";

/** Fed into `EditorAppConfig.languageDef` — that's the only hook the
 * wrapper exposes for registering a language + its Monarch tokenizer. */
export const wxLanguageExtensionConfig: monaco.languages.ILanguageExtensionPoint =
	{
		id: WX_LANGUAGE_ID,
		extensions: [".wx"],
		aliases: ["WX", "wx"],
	};

/** Minimal syntax highlighting for WX — enough to be readable, not a full grammar. */
export const wxMonarchLanguage: monaco.languages.IMonarchLanguage = {
	keywords: [
		"fn",
		"local",
		"mut",
		"if",
		"else",
		"loop",
		"break",
		"continue",
		"return",
		"true",
		"false",
		"struct",
		"enum",
		"impl",
		"trait",
		"global",
		"const",
		"module",
		"memory",
		"import",
		"export",
		"pub",
		"as",
		"type",
		"use",
	],
	typeKeywords: [
		"i8",
		"i16",
		"i32",
		"i64",
		"u8",
		"u16",
		"u32",
		"u64",
		"f32",
		"f64",
		"bool",
		"char",
	],
	tokenizer: {
		root: [
			[/\/\/.*$/, "comment"],
			[/"([^"\\]|\\.)*"/, "string"],
			[/\d+\.\d+/, "number.float"],
			[/\d+/, "number"],
			[
				/[a-zA-Z_]\w*/,
				{
					cases: {
						"@keywords": "keyword",
						"@typeKeywords": "type",
						"@default": "identifier",
					},
				},
			],
		],
	},
};

/** Registers the wx language, its Monarch tokenizer, and its bracket/comment
 * configuration directly against plain `monaco-editor` — previously this was
 * split between `EditorAppConfig.languageDef` (extension point + tokenizer)
 * and this function (everything `languageDef` had no slot for); without that
 * wrapper, it's simplest to just do all of it here in one place. */
export function configureWxLanguage(monacoApi: typeof monaco) {
	monacoApi.languages.register(wxLanguageExtensionConfig);
	monacoApi.languages.setMonarchTokensProvider(WX_LANGUAGE_ID, wxMonarchLanguage);
	monacoApi.languages.setLanguageConfiguration(WX_LANGUAGE_ID, {
		comments: { lineComment: "//" },
		brackets: [
			["{", "}"],
			["[", "]"],
			["(", ")"],
		],
		autoClosingPairs: [
			{ open: "{", close: "}" },
			{ open: "[", close: "]" },
			{ open: "(", close: ")" },
			{ open: '"', close: '"' },
		],
		surroundingPairs: [
			{ open: "{", close: "}" },
			{ open: "[", close: "]" },
			{ open: "(", close: ")" },
		],
	});
}
