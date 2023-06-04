use std::collections::HashMap;

#[derive(Debug)]
pub enum JsonValue {
    Object(JsonObject),
    Array(Vec<JsonValue>),
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

#[derive(Debug)]
pub struct JsonObject {
    pub values: HashMap<String, JsonValue>,
}

#[derive(Debug)]
pub enum JsonValueParseError {
    Empty,
    MalformedTrue,
    MalformedFalse,
    MalformedNull,
    Object(JsonObjectParseError),
    Array(JsonArrayParseError),
    String(JsonStringParseError),
    Number(JsonNumberParseError),
    /// Specifically here to allow the array parser to know if there were no
    /// array contents.
    UnexpectedCloseBracket,
}

#[derive(Debug)]
pub enum JsonObjectParseError {
    NoInitialBrace(Option<char>),
    NoClosingBrace,
    TrailingComma,
    String(JsonStringParseError),
    NoColon(Option<char>),
    UnrecognisedAfterValue(char),
    NothingAfterComma,
    ObjectFailedToParse(Box<JsonValueParseError>),
}

#[derive(Debug)]
pub enum JsonArrayParseError {
    NoInitialBracket(Option<char>),
    Value(Box<JsonValueParseError>),
    EndOfStream,
    NoComma(char),
}

#[derive(Debug)]
pub enum JsonNumberParseError {
    NothingAfterMinus,
    NothingAfterE(Option<char>),
    NonDigit(char),
}

#[derive(Debug)]
pub enum JsonStringParseError {
    /// Contains the first char of the token, when a string was expected; the initial double-quote
    /// was missing.
    NotQuoted(char),
    EndOfFile,
    UnrecognisedEscaped(char),
    UnicodeLiteralTooShort,
    UnicodeLiteralInvalidDigit(char),
    UnescapedControlChar(char),
    UnicodeSurrogateUnsupported(u32),
}

impl From<JsonStringParseError> for JsonObjectParseError {
    fn from(value: JsonStringParseError) -> Self {
        JsonObjectParseError::String(value)
    }
}

impl From<JsonObjectParseError> for JsonValueParseError {
    fn from(value: JsonObjectParseError) -> Self {
        Self::Object(value)
    }
}

impl From<JsonStringParseError> for JsonValueParseError {
    fn from(value: JsonStringParseError) -> Self {
        Self::String(value)
    }
}

impl From<JsonArrayParseError> for JsonValueParseError {
    fn from(value: JsonArrayParseError) -> Self {
        Self::Array(value)
    }
}

impl From<JsonNumberParseError> for JsonValueParseError {
    fn from(value: JsonNumberParseError) -> Self {
        Self::Number(value)
    }
}

impl From<JsonValueParseError> for JsonObjectParseError {
    fn from(value: JsonValueParseError) -> Self {
        Self::ObjectFailedToParse(Box::new(value))
    }
}

/// The JSON spec is clear about what constitutes whitespace.
const fn is_whitespace(c: char) -> bool {
    matches!(c as u32, 9 | 10 | 13 | 32)
}

/// Consume whitespace until the first non-whitespace character, returning that character
/// (or None if we reached the end of the string), and leaving the iterator sitting on that
/// first non-whitespace character.
fn consume_whitespace<I>(iter: &mut I) -> Option<char>
where
    I: Iterator<Item = char>,
{
    iter.by_ref().find(|&c| !is_whitespace(c))
}

/// Leaves the iterator sitting on the first unparsed character, and returns that character as well
/// in the Ok case.
fn consume_number<I>(
    iter: &mut I,
    current: char,
) -> Result<(f64, Option<char>), JsonNumberParseError>
where
    I: Iterator<Item = char>,
{
    let mut current = current;
    let mut negative = 1.0;
    let mut integer_part = 0f64;
    let mut fractional_part = 0f64;

    if current == '-' {
        negative = -1.0;
        current = match iter.next() {
            None => return Err(JsonNumberParseError::NothingAfterMinus),
            Some(c) => c,
        }
    };

    if current != '0' {
        if ('1'..='9').contains(&current) {
            integer_part = (current as u8 - b'0') as f64;
            loop {
                current = match iter.next() {
                    None => return Ok((negative * integer_part, None)),
                    Some(v) => v,
                };
                if current.is_ascii_digit() {
                    integer_part = integer_part * 10.0 + ((current as u8 - b'0') as f64)
                } else {
                    break;
                }
            }
        } else {
            return Err(JsonNumberParseError::NonDigit(current));
        }
    }

    // Parse fraction
    if current == '.' {
        let mut multiplier = 0.1;
        loop {
            current = match iter.next() {
                None => return Ok((negative * (integer_part + fractional_part), None)),
                Some(v) => v,
            };
            if current.is_ascii_digit() {
                fractional_part += multiplier * ((current as u8 - b'0') as f64);
                multiplier *= 0.1;
            } else {
                break;
            }
        }
    }

    // Parse exponent
    if current != 'e' && current != 'E' {
        return Ok((negative * (integer_part + fractional_part), Some(current)));
    }

    let negative_exp = match iter.next() {
        Some('-') => -1.0,
        Some('+') => 1.0,
        c => return Err(JsonNumberParseError::NothingAfterE(c)),
    };
    let mut exponent = 0.0;

    loop {
        current = match iter.next() {
            None => {
                let pow = 10.0_f64.powf(negative_exp * exponent);
                let to_ret = negative * (integer_part + fractional_part) * pow;
                return Ok((to_ret, None));
            }
            Some(v) => v,
        };
        if current.is_ascii_digit() {
            exponent = exponent * 10.0 + ((current as u8 - b'0') as f64);
        } else {
            let pow = 10.0_f64.powf(negative_exp * exponent);
            let to_ret = negative * (integer_part + fractional_part) * pow;
            return Ok((to_ret, Some(current)));
        }
    }
}

/// Advances the input iterator by one.
fn consume_hex_char<I>(iter: &mut I) -> Result<u32, JsonStringParseError>
where
    I: Iterator<Item = char>,
{
    let v = match iter.next() {
        None => return Err(JsonStringParseError::EndOfFile),
        Some(c) => {
            if c.is_ascii_digit() {
                (c as u8) - b'0'
            } else if ('A'..='F').contains(&c) {
                (c as u8) - b'A' + 10
            } else if ('a'..='f').contains(&c) {
                (c as u8) - b'a' + 10
            } else {
                return Err(JsonStringParseError::UnicodeLiteralInvalidDigit(c));
            }
        }
    };
    Ok(v as u32)
}

/// Expects you to have peeled off the first '"' already.
/// In the Ok case, returns with the iterator sitting on the final '"'.
fn consume_string<I>(iter: &mut I) -> Result<String, JsonStringParseError>
where
    I: Iterator<Item = char>,
{
    let mut is_control_character = false;
    let mut result = String::new();

    match iter.next() {
        None => {
            return Err(JsonStringParseError::EndOfFile);
        }
        Some(start_char) => {
            if start_char == '\\' {
                is_control_character = true;
            } else if start_char == '"' {
                return Ok(result);
            } else {
                result.push(start_char);
            }
        }
    };

    loop {
        let char = match iter.next() {
            None => {
                return Err(JsonStringParseError::EndOfFile);
            }
            Some(x) => x,
        };
        if is_control_character {
            is_control_character = false;
            match char {
                '"' | '\\' | '/' => result.push(char),
                'b' => result.push('\x08'),
                'f' => result.push('\x0c'),
                'n' => result.push('\n'),
                'r' => result.push('\r'),
                't' => result.push('\t'),
                'u' => {
                    let mut v = consume_hex_char(iter)?;
                    v = 16 * v + consume_hex_char(iter)?;
                    v = 16 * v + consume_hex_char(iter)?;
                    v = 16 * v + consume_hex_char(iter)?;
                    match char::from_u32(v) {
                        None => {
                            return Err(JsonStringParseError::UnicodeSurrogateUnsupported(v));
                        }
                        Some(c) => {
                            result.push(c);
                        }
                    }
                }
                _ => return Err(JsonStringParseError::UnrecognisedEscaped(char)),
            }
        } else if char == '"' {
            return Ok(result);
        } else if char as u32 <= 31 {
            return Err(JsonStringParseError::UnescapedControlChar(char));
        } else if char == '\\' {
            is_control_character = true;
        } else {
            result.push(char);
        }
    }
}

/// Leaves the iterator sitting on the final ']'. Assumes that the iterator has consumed the first
/// '[' if `expect_bracket` is false.
fn consume_array<I>(
    chars: &mut I,
    expect_bracket: bool,
) -> Result<Vec<JsonValue>, JsonArrayParseError>
where
    I: Iterator<Item = char>,
{
    if expect_bracket {
        match chars.next() {
            None => {
                return Err(JsonArrayParseError::NoInitialBracket(None));
            }
            Some(c) => {
                if c != '[' {
                    return Err(JsonArrayParseError::NoInitialBracket(Some(c)));
                }
            }
        }
    };

    let mut result: Vec<JsonValue> = Vec::new();

    loop {
        let (v, next_char) = match JsonValue::parse_iter(chars, None) {
            Err(JsonValueParseError::UnexpectedCloseBracket) => {
                return Ok(result);
            }
            Err(v) => return Err(JsonArrayParseError::Value(Box::new(v))),
            Ok(r) => r,
        };
        result.push(v);
        match next_char {
            None => {
                return Err(JsonArrayParseError::EndOfStream);
            }
            Some(']') => {
                return Ok(result);
            }
            Some(',') => {}
            Some(c) => return Err(JsonArrayParseError::NoComma(c)),
        }
    }
}

impl JsonValue {
    /// Consumes the JSON value and leaves the iterator sitting on the first non-whitespace
    /// character after the JSON value. Returns that non-whitespace character in the Ok case.
    pub(crate) fn parse_iter<I>(
        chars: &mut I,
        first_char: Option<char>,
    ) -> Result<(JsonValue, Option<char>), JsonValueParseError>
    where
        I: Iterator<Item = char>,
    {
        let first_char = match first_char {
            None => match consume_whitespace(chars) {
                None => {
                    return Err(JsonValueParseError::Empty);
                }
                Some(c) => c,
            },
            Some(first_char) => {
                if is_whitespace(first_char) {
                    match consume_whitespace(chars) {
                        None => {
                            return Err(JsonValueParseError::Empty);
                        }
                        Some(c) => c,
                    }
                } else {
                    first_char
                }
            }
        };

        let (result, next_char) = match first_char {
            '"' => {
                let s = consume_string(chars)?;
                (JsonValue::String(s), chars.next())
            }
            '{' => {
                let o = JsonObject::parse_iter(chars, false)?;
                (JsonValue::Object(o), chars.next())
            }
            '[' => {
                let arr = consume_array(chars, false)?;
                (JsonValue::Array(arr), chars.next())
            }
            't' => {
                if let Some('r') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedTrue);
                }
                if let Some('u') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedTrue);
                }
                if let Some('e') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedTrue);
                }
                (JsonValue::Boolean(true), chars.next())
            }
            'f' => {
                if let Some('a') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedFalse);
                }
                if let Some('l') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedFalse);
                }
                if let Some('s') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedFalse);
                }
                if let Some('e') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedFalse);
                }
                (JsonValue::Boolean(false), chars.next())
            }
            'n' => {
                if let Some('u') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedNull);
                }
                if let Some('l') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedNull);
                }
                if let Some('l') = chars.next() {
                } else {
                    return Err(JsonValueParseError::MalformedNull);
                }
                (JsonValue::Null, chars.next())
            }
            ']' => {
                return Err(JsonValueParseError::UnexpectedCloseBracket);
            }
            current => {
                let (number, next_char) = consume_number(chars, current)?;
                (JsonValue::Number(number), next_char)
            }
        };
        match next_char {
            None => Ok((result, None)),
            Some(next_char) => {
                if is_whitespace(next_char) {
                    Ok((result, consume_whitespace(chars)))
                } else {
                    Ok((result, Some(next_char)))
                }
            }
        }
    }

    pub fn parse<I>(chars: &mut I) -> Result<(JsonValue, Option<char>), JsonValueParseError>
    where
        I: Iterator<Item = char>,
    {
        Self::parse_iter(chars, None)
    }
}

impl JsonObject {
    /// Leaves the iterator sitting on the closing '}'.
    pub(crate) fn parse_iter<I>(
        chars: &mut I,
        expect_brace: bool,
    ) -> Result<JsonObject, JsonObjectParseError>
    where
        I: Iterator<Item = char>,
    {
        if expect_brace {
            match chars.next() {
                None => {
                    return Err(JsonObjectParseError::NoInitialBrace(None));
                }
                Some(c) => {
                    if c != '{' {
                        return Err(JsonObjectParseError::NoInitialBrace(Some(c)));
                    }
                }
            }
        }

        let mut values = HashMap::new();
        let mut has_looped = false;

        loop {
            let key = match consume_whitespace(chars) {
                None => {
                    return Err(JsonObjectParseError::NoClosingBrace);
                }
                Some(c) => {
                    if c == '}' {
                        if has_looped {
                            // There was a trailing comma.
                            return Err(JsonObjectParseError::TrailingComma);
                        } else {
                            // No entries, so it's an empty object.
                            return Ok(JsonObject { values });
                        }
                    } else if c != '"' {
                        return Err(JsonObjectParseError::String(
                            JsonStringParseError::NotQuoted(c),
                        ));
                    }
                    consume_string(chars)?
                }
            };

            match consume_whitespace(chars) {
                None => {
                    return Err(JsonObjectParseError::NoColon(None));
                }
                Some(':') => {}
                Some(c) => return Err(JsonObjectParseError::NoColon(Some(c))),
            }

            let (value, next_char) = JsonValue::parse_iter(chars, None)?;

            values.insert(key, value);

            match next_char {
                Some('}') => return Ok(JsonObject { values }),
                None => return Err(JsonObjectParseError::NoClosingBrace),
                Some(',') => {
                    has_looped = true;
                }
                Some(c) => return Err(JsonObjectParseError::UnrecognisedAfterValue(c)),
            };
        }
    }
}

#[cfg(test)]
mod test {
    use crate::json_object::{
        JsonObject, JsonObjectParseError, JsonStringParseError, JsonValue, JsonValueParseError,
    };

    fn parse_object(s: &str) -> JsonObject {
        match JsonValue::parse(&mut s.chars()) {
            Err(e) => {
                panic!("Expected to parse object, got {:?}", e)
            }
            Ok((obj, None)) => match obj {
                JsonValue::Object(o) => o,
                other => panic!("Unexpectedly not an object: {:?}", other),
            },
            Ok((_, Some(c))) => {
                panic!("Parsed successfully but had leftover char '{c}'");
            }
        }
    }

    #[test]
    fn test_simple() {
        let s = r#"{
  "name": "John Doe",
  "age": 32,
  "isStudent": false,
  "subjects": ["Maths", "English", "Science"]
}"#;

        let obj = parse_object(s);
        if let JsonValue::String(name) = obj.values.get("name").unwrap() {
            assert_eq!(name, "John Doe")
        } else {
            panic!("oh no")
        }
        if let JsonValue::Boolean(is_student) = obj.values.get("isStudent").unwrap() {
            assert_eq!(*is_student, false)
        } else {
            panic!("oh no")
        }
        if let JsonValue::Number(age) = obj.values.get("age").unwrap() {
            assert_eq!(*age, 32.0)
        } else {
            panic!("oh no")
        }
        if let JsonValue::Array(subjects) = obj.values.get("subjects").unwrap() {
            let subjects = subjects
                .iter()
                .map(|v| match v {
                    JsonValue::String(s) => s,
                    _ => panic!("oh no"),
                })
                .collect::<Vec<_>>();
            assert_eq!(subjects, ["Maths", "English", "Science"])
        } else {
            panic!("oh no")
        }
    }

    #[test]
    fn test_nested() {
        let s = r#"
    {
        "employee": {
        "name": "John Doe",
        "age": 32,
        "department": {
            "name": "Sales",
            "location": "West wing"
        }
    }
}"#;
        let obj = parse_object(s);
        let obj = match obj.values.get("employee").unwrap() {
            JsonValue::Object(o) => o,
            _ => panic!("bad object"),
        };

        match obj.values.get("name").unwrap() {
            JsonValue::String(s) => assert_eq!(s, "John Doe"),
            _ => panic!("bad object"),
        };
        match obj.values.get("age").unwrap() {
            JsonValue::Number(age) => assert_eq!(*age, 32.0),
            _ => panic!("bad object"),
        };
        let obj = match obj.values.get("department").unwrap() {
            JsonValue::Object(o) => o,
            _ => panic!("bad object"),
        };

        match obj.values.get("name").unwrap() {
            JsonValue::String(s) => assert_eq!(s, "Sales"),
            _ => panic!("bad object"),
        };
        match obj.values.get("location").unwrap() {
            JsonValue::String(s) => assert_eq!(s, "West wing"),
            _ => panic!("bad object"),
        };
    }

    #[test]
    fn test_array() {
        let s = r#"[
  {
    "firstName": "John",
    "lastName": "Doe"
  },
  {
    "firstName": "Jane",
    "lastName": "Doe"
  }
]
"#;
        let arr = match JsonValue::parse(&mut s.chars()).unwrap() {
            (JsonValue::Array(a), None) => a,
            e => panic!("bad object: {:?}", e),
        };

        let forenames = arr
            .iter()
            .map(|o| match o {
                JsonValue::Object(o) => match o.values.get("lastName").unwrap() {
                    JsonValue::String(s) => {
                        assert_eq!(s, "Doe");
                        match o.values.get("firstName").unwrap() {
                            JsonValue::String(s) => s,
                            _ => panic!("bad object"),
                        }
                    }
                    _ => panic!("bad object"),
                },
                _ => panic!("bad object"),
            })
            .collect::<Vec<_>>();
        assert_eq!(forenames, ["John", "Jane"]);
    }

    #[test]
    fn test_null() {
        let s = r#"
        {
            "firstName": "John",
            "lastName": null
        }"#;

        let o = parse_object(s);

        match o.values.get("firstName").unwrap() {
            JsonValue::String(s) => {
                assert_eq!(s, "John");
            }
            _ => panic!("bad object"),
        }

        match o.values.get("lastName").unwrap() {
            JsonValue::Null => {}
            _ => panic!("expected Null"),
        }
    }

    #[test]
    fn missing_comma() {
        let s = r#"{
  "firstName": "John"
  "lastName": "Doe"
}"#;

        let error = match JsonValue::parse(&mut s.chars()) {
            Err(JsonValueParseError::Object(o)) => o,
            _ => panic!("bad error"),
        };

        match error {
            JsonObjectParseError::UnrecognisedAfterValue('"') => {}
            _ => panic!("bad error"),
        }
    }

    #[test]
    fn extra_comma() {
        let s = r#"{
  "firstName": "John",
  "lastName": "Doe",
}
"#;

        let error = match JsonValue::parse(&mut s.chars()) {
            Err(JsonValueParseError::Object(o)) => o,
            _ => panic!("bad error"),
        };

        match error {
            JsonObjectParseError::TrailingComma => {}
            e => panic!("bad error: {:?}", e),
        }
    }

    #[test]
    fn invalid_quotes() {
        let s = r#"{
  'firstName': 'John',
  'lastName': 'Doe'
}
"#;

        let error = match JsonValue::parse(&mut s.chars()) {
            Err(JsonValueParseError::Object(o)) => o,
            _ => panic!("bad error"),
        };

        match error {
            JsonObjectParseError::String(JsonStringParseError::NotQuoted('\'')) => {}
            _ => panic!("bad error"),
        }
    }

    #[test]
    fn unicode_literal() {
        let s = r#"{
  "name": "John Doe",
  "message": "Hello, world! \u03a9"
}"#;
        let o = parse_object(s);

        match o.values.get("name").unwrap() {
            JsonValue::String(s) => {
                assert_eq!(s, "John Doe");
            }
            _ => panic!("bad object"),
        }

        match o.values.get("message").unwrap() {
            JsonValue::String(s) => {
                assert_eq!(s, "Hello, world! Ω");
            }
            _ => panic!("bad object"),
        }
    }

    #[test]
    fn exponents() {
        let s = r#"{
  "scientific notation": 1.23e-6,
  "largeNumber": 1.2E+6
}"#;
        let o = parse_object(s);

        match o.values.get("scientific notation").unwrap() {
            JsonValue::Number(actual) => {
                let expected = 1.23e-6_f64;
                // Rust chose the float immediately above 1.23e-6; my computation
                // chose the float immediately below.
                let delta = expected - *actual;
                assert_eq!(delta, 2.1175823681357508E-22);
            }
            e => {
                panic!("bad value: {:?}", e)
            }
        }

        match o.values.get("largeNumber").unwrap() {
            JsonValue::Number(n) => {
                assert_eq!(*n, 1.2e6_f64);
            }
            _ => panic!("bad value"),
        }
    }

    #[test]
    fn escaped_character() {
        let s = r#"{
  "stringWithEscapedChars": "Hello, \"World\"!"
}"#;

        let o = parse_object(s);

        match o.values.get("stringWithEscapedChars").unwrap() {
            JsonValue::String(s) => {
                assert_eq!(s, r#"Hello, "World"!"#);
            }
            _ => panic!("bad value"),
        }
    }

    #[test]
    fn special_chars() {
        let s = r#"{
  "stringWithSpecialChars": "Tab:\t, New Line:\n, Carriage Return:\r"
}"#;
        let o = parse_object(s);

        match o.values.get("stringWithSpecialChars").unwrap() {
            JsonValue::String(s) => {
                assert_eq!(s, "Tab:\t, New Line:\n, Carriage Return:\r");
            }
            _ => panic!("bad value"),
        }
    }

    #[test]
    fn unescaped_control() {
        let s = "{ \"stringWithControlChar\": \"Hello,\u{8} world!\" }";

        let error = match JsonValue::parse(&mut s.chars()) {
            Ok(o) => {
                panic!("Unexpectedly parsed: {:?}", o)
            }
            Err(JsonValueParseError::Object(JsonObjectParseError::ObjectFailedToParse(v))) => v,
            Err(e) => {
                panic!("Unexpected error: {:?}", e)
            }
        };

        match error.as_ref() {
            JsonValueParseError::String(JsonStringParseError::UnescapedControlChar('\u{8}')) => {}
            e => {
                panic!("Unexpected error: {:?}", e)
            }
        }
    }
}
