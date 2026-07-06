import {
	createAnsiSequenceParser,
	createColorPalette,
	type Color,
	type ParseToken,
} from "ansi-sequence-parser";
import {
	Disposable,
	Range,
	TextEditor,
	TextEditorDecorationType,
	ThemeColor,
	Uri,
	window,
} from "vscode";

// Scheme for the "click for full compiler diagnostic" virtual documents.
// Shared so extension.ts's listeners can bail out before doing any work for
// the (much more common) case of an edit/open in some unrelated document.
export const DIAGNOSTICS_VIEW_SCHEME = "wx-diagnostics-view";

// Parses ANSI SGR-styled text line by line, using a single stateful parser
// so a style opened on one line and never explicitly reset still applies to
// the following lines (matches how a real terminal would render it).
export function parseAnsiLines(raw: string): ParseToken[][] {
	const parser = createAnsiSequenceParser();
	return raw.split(/\r?\n/).map((line) => parser.parse(line));
}

// Plain-text form of `raw`, escape codes removed — what the virtual
// document's actual text content should be (decorations are painted over
// this separately by `AnsiDecorationProvider`, not embedded in the text).
export function stripAnsi(raw: string): string {
	return parseAnsiLines(raw)
		.map((tokens) => tokens.map((token) => token.value).join(""))
		.join("\n");
}

function capitalize(s: string): string {
	return s.charAt(0).toUpperCase() + s.slice(1);
}

function colorKey(color: Color): string {
	switch (color.type) {
		case "named":
			return `named:${color.name}`;
		case "rgb":
			return `rgb:${color.rgb.join(",")}`;
		case "table":
			return `table:${color.index}`;
	}
}

// Applies `TextEditorDecorationType`s to editors showing a
// `wx-diagnostics-view` virtual document, by re-parsing the same raw ANSI
// text the content provider strips for the document's plain text. Colors
// are resolved to `ThemeColor("terminal.ansi*")` (not literal hex) so the
// view matches whatever terminal theme the user already has configured,
// except for true 24-bit colors and 256-color palette entries, which have
// no theme equivalent and fall back to a literal color string.
export class AnsiDecorationProvider implements Disposable {
	private readonly decorationTypes = new Map<
		string,
		TextEditorDecorationType
	>();
	private readonly colorPalette = createColorPalette();

	constructor(
		private readonly fetchRendered: (uri: Uri) => Promise<string | null>,
	) {}

	dispose(): void {
		for (const decorationType of this.decorationTypes.values()) {
			decorationType.dispose();
		}
		this.decorationTypes.clear();
	}

	async provideDecorations(editor: TextEditor): Promise<void> {
		if (editor.document.uri.scheme !== DIAGNOSTICS_VIEW_SCHEME) return;
		const decorations = await this.computeDecorations(editor.document.uri);
		for (const [decorationType, ranges] of decorations) {
			editor.setDecorations(decorationType, ranges);
		}
	}

	private async computeDecorations(
		uri: Uri,
	): Promise<[TextEditorDecorationType, Range[]][]> {
		const raw = await this.fetchRendered(uri);
		if (raw === null) return [];

		const result = new Map<TextEditorDecorationType, Range[]>();
		// Populate every known decoration type so a span whose style changed
		// (or disappeared) after an edit actually clears instead of leaving a
		// stale decoration painted from a previous render.
		for (const decorationType of this.decorationTypes.values()) {
			result.set(decorationType, []);
		}

		parseAnsiLines(raw).forEach((tokens, lineNumber) => {
			let offset = 0;
			for (const token of tokens) {
				const range = new Range(
					lineNumber,
					offset,
					lineNumber,
					offset + token.value.length,
				);
				offset += token.value.length;

				if (
					!token.foreground &&
					!token.background &&
					token.decorations.size === 0
				) {
					continue;
				}

				const decorationType = this.getDecorationType(token);
				if (!result.has(decorationType)) result.set(decorationType, []);
				result.get(decorationType)!.push(range);
			}
		});

		return [...result];
	}

	private getDecorationType(token: ParseToken): TextEditorDecorationType {
		const key = [
			token.foreground ? colorKey(token.foreground) : "",
			token.background ? colorKey(token.background) : "",
			[...token.decorations].sort().join(","),
		].join("|");

		const existing = this.decorationTypes.get(key);
		if (existing) return existing;

		const decorationType = window.createTextEditorDecorationType({
			color: token.foreground
				? this.resolveColor(token.foreground)
				: undefined,
			backgroundColor: token.background
				? this.resolveColor(token.background)
				: undefined,
			fontWeight: token.decorations.has("bold") ? "bold" : undefined,
			fontStyle: token.decorations.has("italic") ? "italic" : undefined,
			textDecoration: token.decorations.has("underline")
				? "underline"
				: undefined,
		});
		this.decorationTypes.set(key, decorationType);
		return decorationType;
	}

	private resolveColor(color: Color): ThemeColor | string {
		switch (color.type) {
			case "named":
				return new ThemeColor(`terminal.ansi${capitalize(color.name)}`);
			case "rgb":
				return `rgb(${color.rgb.join(",")})`;
			case "table":
				return this.colorPalette.value(color);
		}
	}
}
