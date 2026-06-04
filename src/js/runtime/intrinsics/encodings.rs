use crate::{
    common::unicode::{get_hex_value, CodePoint},
    must,
    runtime::{
        error::{syntax_error_value, type_error},
        get,
        heap_item_descriptor::HeapItemKind,
        numeric_constants::MAX_SAFE_INTEGER_U64,
        object_value::ObjectValue,
        ordinary_object::object_create_with_optional_proto,
        string_value::{FlatString, StringValue},
        type_utilities::to_boolean,
        Context, EvalResult, Handle, Value,
    },
};

/// Encode a sequence of bytes as hex code points.
pub fn encode_hex(input: &[u8]) -> Vec<u8> {
    let mut encoded = Vec::with_capacity(input.len() * 2);

    for &byte in input {
        let high_nibble = byte >> 4;
        let low_nibble = byte & 0x0F;

        encoded.push(nibble_to_hex_code_point(high_nibble));
        encoded.push(nibble_to_hex_code_point(low_nibble));
    }

    encoded
}

/// Convert a 4-bit nibble to the code point for its hex representation (lowercase).
fn nibble_to_hex_code_point(nibble: u8) -> u8 {
    if nibble < 10 {
        b'0' + nibble
    } else {
        b'a' + (nibble - 10)
    }
}

pub struct DecodeResult {
    pub bytes: Vec<u8>,
    pub read: u32,
    pub error: Option<Handle<Value>>,
}

/// Decode a full hex string into bytes following the FromHex method of the spec.
pub fn decode_hex(
    cx: Context,
    string: Handle<StringValue>,
    max_length: Option<u64>,
    full_method_name: &str,
) -> EvalResult<DecodeResult> {
    let max_length = max_length.unwrap_or(MAX_SAFE_INTEGER_U64);
    let mut read = 0;
    let mut bytes = vec![];

    let string = string.flatten()?;

    if string.len() % 2 != 0 {
        let error = syntax_error_value(
            cx,
            &format!("{} argument must have an even length", full_method_name),
        )?;

        return Ok(DecodeResult { bytes, read, error: Some(error) });
    }

    while read < string.len() && (bytes.len() as u64) < max_length {
        let high_nibble_opt = get_hex_value(string.code_unit_at(read) as CodePoint);
        let low_nibble_opt = get_hex_value(string.code_unit_at(read + 1) as CodePoint);

        let (Some(high_nibble), Some(low_nibble)) = (high_nibble_opt, low_nibble_opt) else {
            let error = syntax_error_value(
                cx,
                &format!("{} argument must only contain hexadecimal digits", full_method_name),
            )?;
            return Ok(DecodeResult { bytes, read, error: Some(error) });
        };

        let byte = ((high_nibble as u8) << 4) | (low_nibble as u8);
        bytes.push(byte);

        read += 2;
    }

    Ok(DecodeResult { bytes, read, error: None })
}

#[derive(Clone, Copy, PartialEq)]
pub enum Base64Alphabet {
    Standard,
    UrlSafe,
}

impl Base64Alphabet {
    const STANDARD: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    const URL_SAFE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    /// Return the base64 conversion for a 6-bit value.
    const fn encode(self, bits: u8) -> u8 {
        match self {
            Base64Alphabet::Standard => Self::STANDARD[bits as usize],
            Base64Alphabet::UrlSafe => Self::URL_SAFE[bits as usize],
        }
    }
}

fn is_standard_base64_code_unit(code_unit: u16) -> bool {
    (b'A' as u16..=b'Z' as u16).contains(&code_unit)
        || (b'a' as u16..=b'z' as u16).contains(&code_unit)
        || (b'0' as u16..=b'9' as u16).contains(&code_unit)
        || code_unit == b'+' as u16
        || code_unit == b'/' as u16
}

#[derive(Clone, Copy, PartialEq)]
pub enum LastChunkHandling {
    Loose,
    Strict,
    StopBeforePartial,
}

/// Encode a sequence of bytes as base64 code points using the given options.
pub fn encode_base64(input: &[u8], alphabet: Base64Alphabet, omit_padding: bool) -> Vec<u8> {
    let length = input.len();

    // Base64 encodes 3 bytes triplets into 4 code points. Round up to a full triplet for the
    // remaining 1 or 2 bytes.
    let num_byte_triplets = length.div_ceil(3);
    let mut output = Vec::with_capacity(num_byte_triplets * 4);

    let mut i = 0;

    while i < length {
        let first_byte = input[i];

        // First 6 bits form first code point, remaining 2 bits are part of the second
        let first_bits = first_byte >> 2;
        let second_bits_high = (first_byte & 0b11) << 4;
        output.push(alphabet.encode(first_bits));

        let second_byte = if i + 1 < length {
            input[i + 1]
        } else {
            // If last triplet is incomplete, encode remaining bits and add padding to 4 bytes
            output.push(alphabet.encode(second_bits_high));

            if !omit_padding {
                output.push(b'=');
                output.push(b'=');
            }

            break;
        };

        // First 4 bits complete second code point, remaining 4 bits are part of the third
        let second_bits = second_bits_high | (second_byte >> 4);
        let third_bits_high = (second_byte & 0b1111) << 2;
        output.push(alphabet.encode(second_bits));

        let third_byte = if i + 2 < length {
            input[i + 2]
        } else {
            // If last triplet is incomplete, encode remaining bits and add padding to 4 bytes
            output.push(alphabet.encode(third_bits_high));

            if !omit_padding {
                output.push(b'=');
            }

            break;
        };

        // First 2 bits complete third code point, remaining 6 bits are the fourth
        let third_bits = third_bits_high | (third_byte >> 6);
        let fourth_bits = third_byte & 0b111111;
        output.push(alphabet.encode(third_bits));
        output.push(alphabet.encode(fourth_bits));

        i += 3;
    }

    output
}

/// Decode a full base64 string into bytes following the FromBase64 method of the spec.
///
/// FromBase64 (https://tc39.es/proposal-arraybuffer-base64/spec/#sec-frombase64)
pub fn decode_base64(
    cx: Context,
    string: Handle<StringValue>,
    alphabet: Base64Alphabet,
    last_chunk_handling: LastChunkHandling,
    max_length: Option<u64>,
    full_method_name: &str,
) -> EvalResult<DecodeResult> {
    let max_length = max_length.unwrap_or(MAX_SAFE_INTEGER_U64);

    let mut bytes = vec![];
    let mut read = 0;

    if max_length == 0 {
        return Ok(DecodeResult { bytes, read, error: None });
    }

    let string = string.flatten()?;
    let length = string.len();

    let mut i = 0;
    let mut chunk = vec![];

    loop {
        i = skip_ascii_whitespace(string, i);

        if i == length {
            if !chunk.is_empty() {
                match last_chunk_handling {
                    LastChunkHandling::StopBeforePartial => {
                        return Ok(DecodeResult { bytes, read, error: None })
                    }
                    LastChunkHandling::Loose => {
                        if chunk.len() == 1 {
                            let error = syntax_error_value(
                                cx,
                                &format!(
                                    "{} argument has incomplete trailing base64 data",
                                    full_method_name
                                ),
                            )?;
                            return Ok(DecodeResult { bytes, read, error: Some(error) });
                        }

                        let decoded_result =
                            must!(decode_final_base64_chunk(cx, &chunk, false, full_method_name));
                        let DecodeChunkResult::Ok(mut decoded) = decoded_result else {
                            unreachable!()
                        };

                        bytes.append(&mut decoded);
                    }
                    LastChunkHandling::Strict => {
                        let error = syntax_error_value(
                            cx,
                            &format!(
                                "{} argument has incomplete trailing base64 data",
                                full_method_name
                            ),
                        )?;
                        return Ok(DecodeResult { bytes, read, error: Some(error) });
                    }
                }
            }

            return Ok(DecodeResult { bytes, read: length, error: None });
        }

        let mut code_unit = string.code_unit_at(i);
        i += 1;

        if code_unit == b'=' as u16 {
            if chunk.len() < 2 {
                let error = syntax_error_value(
                    cx,
                    &format!("{} argument has invalid padding", full_method_name),
                )?;
                return Ok(DecodeResult { bytes, read, error: Some(error) });
            }

            i = skip_ascii_whitespace(string, i);

            if chunk.len() == 2 {
                if i == length {
                    if last_chunk_handling == LastChunkHandling::StopBeforePartial {
                        return Ok(DecodeResult { bytes, read, error: None });
                    } else {
                        let error = syntax_error_value(
                            cx,
                            &format!("{} argument has invalid padding", full_method_name),
                        )?;
                        return Ok(DecodeResult { bytes, read, error: Some(error) });
                    }
                }

                if string.code_unit_at(i) == b'=' as u16 {
                    i = skip_ascii_whitespace(string, i + 1);
                }
            }

            if i < length {
                let error = syntax_error_value(
                    cx,
                    &format!("{} argument has invalid padding", full_method_name),
                )?;
                return Ok(DecodeResult { bytes, read, error: Some(error) });
            }

            let throw_on_extra_bits = last_chunk_handling == LastChunkHandling::Strict;

            let decode_chunk_result =
                decode_final_base64_chunk(cx, &chunk, throw_on_extra_bits, full_method_name)?;
            let mut decoded = match decode_chunk_result {
                DecodeChunkResult::Ok(decoded) => decoded,
                DecodeChunkResult::DecodeError(error) => {
                    return Ok(DecodeResult { bytes, read, error: Some(error) });
                }
            };

            bytes.append(&mut decoded);

            return Ok(DecodeResult { bytes, read: length, error: None });
        }

        if alphabet == Base64Alphabet::UrlSafe {
            if code_unit == b'+' as u16 || code_unit == b'/' as u16 {
                let error = syntax_error_value(
                    cx,
                    &format!(
                        "{} argument has invalid character for 'base64url' encoding",
                        full_method_name
                    ),
                )?;
                return Ok(DecodeResult { bytes, read, error: Some(error) });
            } else if code_unit == b'-' as u16 {
                code_unit = b'+' as u16;
            } else if code_unit == b'_' as u16 {
                code_unit = b'/' as u16;
            }
        }

        if !is_standard_base64_code_unit(code_unit) {
            let error = syntax_error_value(
                cx,
                &format!("{} argument has invalid character", full_method_name),
            )?;
            return Ok(DecodeResult { bytes, read, error: Some(error) });
        }

        let num_remaining_bytes = max_length - bytes.len() as u64;
        if (num_remaining_bytes == 1 && chunk.len() == 2)
            || (num_remaining_bytes == 2 && chunk.len() == 3)
        {
            return Ok(DecodeResult { bytes, read, error: None });
        }

        chunk.push(code_unit as u8);

        if chunk.len() == 4 {
            let mut decoded = decode_full_length_base64_chunk(&chunk);
            bytes.append(&mut decoded);

            chunk.clear();
            read = i;

            if bytes.len() as u64 == max_length {
                return Ok(DecodeResult { bytes, read, error: None });
            }
        }
    }
}

fn skip_ascii_whitespace(string: Handle<FlatString>, index: u32) -> u32 {
    let mut index = index;
    while index < string.len() {
        let code_unit = string.code_unit_at(index);
        if !matches!(code_unit, 0x09 | 0x0A | 0x0C | 0x0D | 0x20) {
            break;
        }

        index += 1;
    }

    index
}

enum DecodeChunkResult {
    Ok(Vec<u8>),
    DecodeError(Handle<Value>),
}

/// DecodeFinalBase64Chunk (https://tc39.es/ecma262/#sec-decodefinalbase64chunk)
fn decode_final_base64_chunk(
    cx: Context,
    chunk: &[u8],
    throw_on_extra_bits: bool,
    full_method_name: &str,
) -> EvalResult<DecodeChunkResult> {
    let original_chunk_length = chunk.len();

    let mut padded_chunk = [chunk[0], chunk[1], b'A', b'A'];
    if original_chunk_length == 3 {
        padded_chunk[2] = chunk[2];
    }

    let bytes = decode_full_length_base64_chunk(&padded_chunk);

    if throw_on_extra_bits
        && ((original_chunk_length == 2 && bytes[1] != 0)
            || (original_chunk_length == 3 && bytes[2] != 0))
    {
        let error = syntax_error_value(
            cx,
            &format!("{} argument has incomplete trailing base64 data", full_method_name),
        )?;
        return Ok(DecodeChunkResult::DecodeError(error));
    }

    if original_chunk_length == 2 {
        Ok(DecodeChunkResult::Ok(vec![bytes[0]]))
    } else {
        Ok(DecodeChunkResult::Ok(vec![bytes[0], bytes[1]]))
    }
}

/// DecodeFullLengthBase64Chunk (https://tc39.es/ecma262/#sec-decodefulllengthbase64chunk)
fn decode_full_length_base64_chunk(chunk: &[u8]) -> Vec<u8> {
    debug_assert!(chunk.len() == 4);

    let first_bits = decode_base64_code_unit_to_bits(chunk[0]);
    let second_bits = decode_base64_code_unit_to_bits(chunk[1]);
    let third_bits = decode_base64_code_unit_to_bits(chunk[2]);
    let fourth_bits = decode_base64_code_unit_to_bits(chunk[3]);

    let first_byte = (first_bits << 2) | (second_bits >> 4);
    let second_byte = (second_bits << 4) | (third_bits >> 2);
    let third_byte = (third_bits << 6) | fourth_bits;

    vec![first_byte, second_byte, third_byte]
}

/// Decode a single valid base64 code unit into its 6 bits of data.
fn decode_base64_code_unit_to_bits(byte: u8) -> u8 {
    match byte {
        b'A'..=b'Z' => byte - b'A',
        b'a'..=b'z' => byte - b'a' + 26,
        b'0'..=b'9' => byte - b'0' + 52,
        b'+' => 62,
        b'/' => 63,
        _ => unreachable!(),
    }
}

pub fn get_base64_options_argument(
    cx: Context,
    argument: Handle<Value>,
    full_method_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    if argument.is_undefined() {
        // Simple object with no prototype
        return Ok(object_create_with_optional_proto::<ObjectValue>(
            cx,
            HeapItemKind::OrdinaryObject,
            None,
        )?
        .to_handle());
    }

    if !argument.is_object() {
        return type_error(cx, &format!("{} options argument must be an object", full_method_name));
    }

    Ok(argument.as_object())
}

pub fn get_base64_alphabet_option(
    cx: Context,
    options: Handle<ObjectValue>,
    full_method_name: &str,
) -> EvalResult<Base64Alphabet> {
    let alphabet_value = get(cx, options, cx.names.alphabet())?;

    if alphabet_value.is_undefined() {
        return Ok(Base64Alphabet::Standard);
    }

    if alphabet_value.is_string() {
        let flatten = alphabet_value.as_string().flatten()?;
        if flatten.eq_str("base64") {
            return Ok(Base64Alphabet::Standard);
        } else if flatten.eq_str("base64url") {
            return Ok(Base64Alphabet::UrlSafe);
        }
    }

    type_error(
        cx,
        &format!("{} alphabet option must be 'base64' or 'base64url'", full_method_name),
    )
}

pub fn get_base64_omit_padding_option(
    cx: Context,
    options: Handle<ObjectValue>,
) -> EvalResult<bool> {
    let omit_padding_value = get(cx, options, cx.names.omit_padding())?;
    Ok(to_boolean(*omit_padding_value))
}

pub fn get_base64_last_chunk_handling_option(
    cx: Context,
    options: Handle<ObjectValue>,
    full_method_name: &str,
) -> EvalResult<LastChunkHandling> {
    let last_chunk_handling_value = get(cx, options, cx.names.last_chunk_handling())?;

    if last_chunk_handling_value.is_undefined() {
        return Ok(LastChunkHandling::Loose);
    }

    if last_chunk_handling_value.is_string() {
        let string = last_chunk_handling_value.as_string().flatten()?;
        if string.eq_str("loose") {
            return Ok(LastChunkHandling::Loose);
        } else if string.eq_str("strict") {
            return Ok(LastChunkHandling::Strict);
        } else if string.eq_str("stop-before-partial") {
            return Ok(LastChunkHandling::StopBeforePartial);
        }
    }

    type_error(
        cx,
        &format!(
            "{} lastChunkHandling option must be 'loose', 'strict', or 'stop-before-partial'",
            full_method_name
        ),
    )
}
