// Run: node doom/test_smoke.mjs
import { readFileSync } from "fs";

const wasm = await WebAssembly.instantiate(readFileSync("smoke.wasm"));
const { memory, check_wad_magic, read_u16_le, read_i32_le, fb_ptr, set_pixel, clear } =
  wasm.instance.exports;

const mem = new Uint8Array(memory.buffer);
let ok = true;

function assert(cond, msg) {
  if (!cond) { console.error("FAIL:", msg); ok = false; }
  else console.log("PASS:", msg);
}

// --- check_wad_magic ---
// Write "IWAD" bytes into memory at address 0, then pass a slice {ptr=0, len=4}.
// A slice is represented as two i32 values on the stack, but exported functions
// receive them as two i32 parameters (ptr, len).
// Actually the function signature takes heap::[]u8 which is an aggregate.
// Let's see what the export looks like.
console.log("\n=== Exported functions ===");
for (const [k, v] of Object.entries(wasm.instance.exports)) {
  if (typeof v === "function") console.log(" ", k, v.length, "params");
}

// Write IWAD magic at address 100
mem[100] = 0x49; // I
mem[101] = 0x57; // W
mem[102] = 0x41; // A
mem[103] = 0x44; // D
// slice = {ptr=100, len=4} — passed as two i32s
const magic_ok = check_wad_magic(100, 4);
assert(magic_ok === 1, `check_wad_magic("IWAD") = ${magic_ok}`);

// Write "PWAD" — should fail
mem[200] = 0x50; // P
mem[201] = 0x57; // W
mem[202] = 0x41; // A
mem[203] = 0x44; // D
const magic_fail = check_wad_magic(200, 4);
assert(magic_fail === 0, `check_wad_magic("PWAD") = ${magic_fail}`);

// --- read_u16_le ---
// Write 0x1234 as little-endian at address 300
mem[300] = 0x34;
mem[301] = 0x12;
const u16 = read_u16_le(300, 2, 0); // ptr=300, len=2, off=0
assert(u16 === 0x1234, `read_u16_le([0x34,0x12], 0) = 0x${u16.toString(16)} (expected 0x1234)`);

// --- read_i32_le ---
// Write 0x12345678 as little-endian at address 400
mem[400] = 0x78;
mem[401] = 0x56;
mem[402] = 0x34;
mem[403] = 0x12;
const i32val = read_i32_le(400, 4, 0); // ptr=400, len=4, off=0
assert(i32val === 0x12345678, `read_i32_le = 0x${(i32val >>> 0).toString(16)} (expected 0x12345678)`);

// --- set_pixel ---
const fbBase = fb_ptr();
console.log(`\nfb_ptr() = ${fbBase}`);
set_pixel(0, 0, 255, 0, 128); // red=255, green=0, blue=128 at (0,0)
assert(mem[fbBase] === 255, `pixel[0].r = ${mem[fbBase]}`);
assert(mem[fbBase + 1] === 0,   `pixel[0].g = ${mem[fbBase + 1]}`);
assert(mem[fbBase + 2] === 128, `pixel[0].b = ${mem[fbBase + 2]}`);

// pixel (1, 0) = RGB(10, 20, 30)
set_pixel(1, 0, 10, 20, 30);
assert(mem[fbBase + 3] === 10, `pixel[1].r = ${mem[fbBase + 3]}`);
assert(mem[fbBase + 4] === 20, `pixel[1].g = ${mem[fbBase + 4]}`);
assert(mem[fbBase + 5] === 30, `pixel[1].b = ${mem[fbBase + 5]}`);

// --- clear ---
clear(99, 88, 77);
assert(mem[fbBase] === 99, `after clear: pixel[0].r = ${mem[fbBase]}`);
assert(mem[fbBase + 1] === 88, `after clear: pixel[0].g = ${mem[fbBase + 1]}`);
// check a pixel further along (x=319, y=199 = last pixel)
const lastOff = (199 * 320 + 319) * 3;
assert(mem[fbBase + lastOff] === 99,     `last pixel.r = ${mem[fbBase + lastOff]}`);
assert(mem[fbBase + lastOff + 1] === 88, `last pixel.g = ${mem[fbBase + lastOff + 1]}`);
assert(mem[fbBase + lastOff + 2] === 77, `last pixel.b = ${mem[fbBase + lastOff + 2]}`);

console.log(ok ? "\n✓ all tests passed" : "\n✗ some tests FAILED");
process.exit(ok ? 0 : 1);
