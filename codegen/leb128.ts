/**
 * High-performance LEB128 (Little Endian Base 128) implementation for WebAssembly
 *
 * LEB128 is a variable-length encoding used in WebAssembly binary format for integers.
 * This implementation provides both unsigned (uleb128) and signed (sleb128) variants.
 */

/**
 * Encodes an unsigned integer into ULEB128 format
 * @param value - The unsigned integer to encode
 * @returns Uint8Array containing the ULEB128 encoded bytes
 */
export function encodeULEB128(value: number): Uint8Array {
	// For small values, pre-allocate a small buffer
	const bytes: number[] = [];

	do {
		let byte = value & 0x7f; // Get 7 least significant bits
		value >>>= 7; // Unsigned right shift by 7 bits

		// If there's more data to encode, set the high bit
		if (value !== 0) {
			byte |= 0x80;
		}

		bytes.push(byte);
	} while (value !== 0);

	return new Uint8Array(bytes);
}

/**
 * Optimized version that works with a pre-allocated buffer
 * @param value - The unsigned integer to encode
 * @param buffer - Pre-allocated buffer to write to
 * @param offset - Starting offset in the buffer
 * @returns Number of bytes written
 */
export function encodeULEB128ToBuffer(
	value: number,
	buffer: Uint8Array,
	offset: number = 0
): number {
	let position = offset;

	do {
		let byte = value & 0x7f;
		value >>>= 7;

		if (value !== 0) {
			byte |= 0x80;
		}

		buffer[position++] = byte;
	} while (value !== 0);

	return position - offset;
}

/**
 * Encodes a signed integer into SLEB128 format
 * @param value - The signed integer to encode
 * @returns Uint8Array containing the SLEB128 encoded bytes
 */
export function encodeSLEB128(value: number): Uint8Array {
	const bytes: number[] = [];
	let more = true;

	while (more) {
		let byte = value & 0x7f;
		value >>= 7; // Signed right shift

		// If value is 0 and most significant bit of byte is not set, or
		// if value is -1 and most significant bit of byte is set, then done
		if (
			(value === 0 && (byte & 0x40) === 0) ||
			(value === -1 && (byte & 0x40) !== 0)
		) {
			more = false;
		} else {
			byte |= 0x80; // More bytes to come
		}

		bytes.push(byte);
	}

	return new Uint8Array(bytes);
}

/**
 * Optimized SLEB128 encoding with pre-allocated buffer
 * @param value - The signed integer to encode
 * @param buffer - Pre-allocated buffer to write to
 * @param offset - Starting offset in the buffer
 * @returns Number of bytes written
 */
export function encodeSLEB128ToBuffer(
	value: number,
	buffer: Uint8Array,
	offset: number = 0
): number {
	let position = offset;
	let more = true;

	while (more) {
		let byte = value & 0x7f;
		value >>= 7;

		if (
			(value === 0 && (byte & 0x40) === 0) ||
			(value === -1 && (byte & 0x40) !== 0)
		) {
			more = false;
		} else {
			byte |= 0x80;
		}

		buffer[position++] = byte;
	}

	return position - offset;
}

/**
 * Decodes an unsigned LEB128 value from a Uint8Array
 * @param bytes - The byte array containing LEB128 data
 * @param offset - Starting position in the array
 * @returns Object containing the decoded value and number of bytes read
 */
export function decodeULEB128(
	bytes: Uint8Array,
	offset: number = 0
): { value: number; bytesRead: number } {
	let result = 0;
	let shift = 0;
	let bytesRead = 0;
	let byte;

	do {
		byte = bytes[offset + bytesRead]!;
		bytesRead++;

		// Only use the 7 least significant bits
		result |= (byte & 0x7f) << shift;
		shift += 7;

		// JavaScript bitwise operations are 32-bit - prevent overflow
		if (shift >= 32) {
			throw new Error(
				"ULEB128 value exceeds JavaScript's 32-bit integer range"
			);
		}
	} while (byte & 0x80);

	return { value: result, bytesRead };
}

/**
 * Decodes a signed LEB128 value from a Uint8Array
 * @param bytes - The byte array containing LEB128 data
 * @param offset - Starting position in the array
 * @returns Object containing the decoded value and number of bytes read
 */
export function decodeSLEB128(
	bytes: Uint8Array,
	offset: number = 0
): { value: number; bytesRead: number } {
	let result = 0;
	let shift = 0;
	let bytesRead = 0;
	let byte;

	do {
		byte = bytes[offset + bytesRead]!;
		bytesRead++;

		// Add the current byte contribution
		result |= (byte & 0x7f) << shift;
		shift += 7;

		// JavaScript bitwise operations are 32-bit - prevent overflow
		if (shift >= 32) {
			throw new Error(
				"SLEB128 value exceeds JavaScript's 32-bit integer range"
			);
		}
	} while (byte & 0x80);

	// Sign extend if necessary
	if (shift < 32 && byte & 0x40) {
		result |= ~0 << shift;
	}

	return { value: result, bytesRead };
}

/**
 * Measures how many bytes an unsigned integer would take when LEB128 encoded
 * @param value - The unsigned integer to measure
 * @returns Number of bytes needed
 */
export function measureULEB128(value: number): number {
	let count = 0;
	do {
		count++;
		value >>>= 7;
	} while (value !== 0);
	return count;
}

/**
 * Measures how many bytes a signed integer would take when LEB128 encoded
 * @param value - The signed integer to measure
 * @returns Number of bytes needed
 */
export function measureSLEB128(value: number): number {
	let count = 0;
	let more = true;

	while (more) {
		const byte = value & 0x7f;
		value >>= 7;

		if (
			(value === 0 && (byte & 0x40) === 0) ||
			(value === -1 && (byte & 0x40) !== 0)
		) {
			more = false;
		}

		count++;
	}

	return count;
}

/**
 * Fast batch encoding of multiple unsigned integers
 * @param values - Array of unsigned integers to encode
 * @returns Uint8Array containing all encoded values
 */
export function batchEncodeULEB128(values: number[]): Uint8Array {
	// First pass: measure total size needed
	let totalSize = 0;
	for (let i = 0; i < values.length; i++) {
		totalSize += measureULEB128(values[i]!);
	}

	// Allocate exact buffer size
	const buffer = new Uint8Array(totalSize);
	let position = 0;

	// Second pass: encode all values
	for (let i = 0; i < values.length; i++) {
		position += encodeULEB128ToBuffer(values[i]!, buffer, position);
	}

	return buffer;
}
