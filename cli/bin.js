#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import { createRequire } from "node:module";

const PLATFORM_PACKAGES = {
	"darwin-arm64": "@wx-lang/cli-darwin-arm64",
	"darwin-x64": "@wx-lang/cli-darwin-x64",
	"linux-x64": "@wx-lang/cli-linux-x64",
	"win32-x64": "@wx-lang/cli-win32-x64",
};

const key = `${process.platform}-${process.arch}`;
const pkgName = PLATFORM_PACKAGES[key];
if (!pkgName) {
	console.error(`wx: unsupported platform "${key}"`);
	process.exit(1);
}

const require = createRequire(import.meta.url);
const binName = process.platform === "win32" ? "wx.exe" : "wx";

let binPath;
try {
	binPath = require.resolve(`${pkgName}/bin/${binName}`);
} catch {
	console.error(
		`wx: could not find binary for "${key}". The optional dependency "${pkgName}" may have failed to install.`,
	);
	process.exit(1);
}

const result = spawnSync(binPath, process.argv.slice(2), { stdio: "inherit" });
if (result.error) {
	console.error(`wx: failed to run binary: ${result.error.message}`);
	process.exit(1);
}
process.exit(result.status ?? 1);
