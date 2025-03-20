export function unescape_string_literal(input: string): string {
	return input
		.slice(1, -1)
		.replace(/\\["\\bfnrt]|\\u2028|\\u2029/g, (chars) => {
			switch (chars) {
				case '\\"':
				case "\\\\":
					return chars[1]!;
				case "\\b":
					return "\b";
				case "\\f":
					return "\f";
				case "\\n":
					return "\n";
				case "\\r":
					return "\r";
				case "\\t":
					return "\t";
				case "\\u2028":
					return "\u2028";
				case "\\u2029":
					return "\u2029";
			}
			throw new Error("unreachable");
		});
}
