#!/bin/bash
# Compile .wx file to .wasm and optimize with wasm-opt

if [ $# -eq 0 ]; then
    echo "Usage: $0 <input.wx> [optimization-level]"
    echo "  optimization-level: -O0, -O1, -O2, -O3, -O4, -Os, -Oz (default: -O3)"
    exit 1
fi

INPUT_FILE="$1"
OPT_LEVEL="${2:--O3}"

if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: File '$INPUT_FILE' not found"
    exit 1
fi

# Extract filename without extension
BASENAME=$(basename "$INPUT_FILE" .wx)
OUTPUT_WASM="${BASENAME}.wasm"
OPTIMIZED_WASM="${BASENAME}.optimized.wasm"

echo "=== Compiling $INPUT_FILE ==="
./target/release/wx-compiler "$INPUT_FILE"

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo ""
echo "=== Optimizing with wasm-opt $OPT_LEVEL ==="
wasm-opt $OPT_LEVEL "$OUTPUT_WASM" -o "$OPTIMIZED_WASM"

if [ $? -ne 0 ]; then
    echo "Optimization failed!"
    exit 1
fi

echo ""
echo "=== Results ==="
ORIGINAL_SIZE=$(stat -c%s "$OUTPUT_WASM")
OPTIMIZED_SIZE=$(stat -c%s "$OPTIMIZED_WASM")
SAVED=$((ORIGINAL_SIZE - OPTIMIZED_SIZE))
PERCENT=$((100 - OPTIMIZED_SIZE * 100 / ORIGINAL_SIZE))

echo "Original:  $OUTPUT_WASM ($ORIGINAL_SIZE bytes)"
echo "Optimized: $OPTIMIZED_WASM ($OPTIMIZED_SIZE bytes)"
echo "Saved:     $SAVED bytes ($PERCENT% reduction)"
echo ""
echo "✓ Done! Use $OPTIMIZED_WASM for production"
