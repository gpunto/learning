use core::panic;

fn main() {
    let input = "abcdefgh";
    let encoded = base64_encode(input.as_bytes());
    let decoded = base64_decode(&encoded);

    println!("{}", encoded);
    println!("{}", String::from_utf8(decoded).unwrap());
}

fn base64_encode(input: &[u8]) -> String {
    return input.chunks(3).map(encode_chunk).collect();
}

fn base64_decode(input: &str) -> Vec<u8> {
    return input.as_bytes().chunks(4).flat_map(decode_chunk).collect();
}

fn encode_chunk(chunk: &[u8]) -> String {
    let mut buffer = 0u32;
    let mut buffer_size = 0;

    for &b in chunk {
        buffer = buffer << 8 | b as u32;
        buffer_size += 8;
    }

    let mut result = String::with_capacity(4);

    while buffer_size >= 6 {
        buffer_size -= 6;
        let word = (buffer >> buffer_size) as u8 & 0b111111;
        result.push(encode_word(word));
    }

    if buffer_size > 0 {
        let word = (buffer << (6 - buffer_size)) as u8 & 0b111111;
        result.push(encode_word(word));
    }

    while result.len() < 4 {
        result.push('=');
    }

    result
}

fn decode_chunk(chunk: &[u8]) -> Vec<u8> {
    // TODO: should we accept non-padded strings?
    if chunk.len() != 4 {
        panic!("Invalid input")
    }

    let mut buffer = 0u32;

    for &word in chunk {
        let decoded = decode_word(word);
        buffer = buffer << 6 | decoded as u32;
    }

    let mut result = Vec::<u8>::new();

    for i in 1..=4 {
        result.push((buffer >> (32 - i * 8)) as u8);
    }

    result
}

fn encode_word(word: u8) -> char {
    match word {
        0..=25 => (b'A' + word) as char,
        26..=51 => (b'a' + word - 26) as char,
        52..=61 => (b'0' + word - 52) as char,
        62 => '+',
        63 => '/',
        _ => panic!("Invalid 6-bit word"),
    }
}

fn decode_word(word: u8) -> u8 {
    match word {
        b'A'..=b'Z' => word - b'A',
        b'a'..=b'z' => 26 + word - b'a',
        b'0'..=b'9' => 52 + word - b'0',
        b'+' => 62,
        b'/' => 63,
        b'=' => 0,
        _ => panic!("Invalid Base64 word"),
    }
}
