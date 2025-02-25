//! A simple varint encoding, optimized for space. Identical to the varint encoding used in
//! protobufs.
//!
//! High bit signals if there are more bytes to read. Lower 7 bits are the payload. Concatenate
//! all 7 byte payloads in a sequence to get the full number.

/// Encode a value as a varint and append it to the given buffer.
pub fn encode_varint(buf: &mut Vec<u8>, value: usize) {
    let mut value = value as u64;

    // Write 7 bits at a time, starting with the lowest bits of the value
    while value >= 0x80 {
        buf.push((value as u8) | 0x80);
        value >>= 7;
    }

    buf.push(value as u8);
}

/// Decode a varint from the given buffer. Return the decoded value as well as its length in bytes.
pub fn decode_varint(buf: &[u8]) -> (usize, usize) {
    let mut value = 0;
    let mut shift = 0;
    let mut i = 0;

    while i < buf.len() {
        // Extract the current byte and add its 7 bit payload as the highest bits of the value
        let byte = buf[i];
        value |= ((byte & 0x7F) as usize) << shift;
        shift += 7;

        i += 1;

        // Only continue to next byte if high bit is set
        if byte & 0x80 == 0 {
            return (value, i);
        }
    }

    panic!("varint did not terminate");
}
