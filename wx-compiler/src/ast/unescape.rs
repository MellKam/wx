#[derive(Debug, PartialEq)]
pub enum UnescapeError {
    IncompleteStr { pos: usize },
    InvalidChar { pos: usize, char: char },
}

impl std::error::Error for UnescapeError {}
impl std::fmt::Display for UnescapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnescapeError::IncompleteStr { pos } => write!(f, "incomplete str, break at {pos}"),
            UnescapeError::InvalidChar { char, pos } => {
                write!(f, "invalid char, {char:?} break at {pos}")
            }
        }
    }
}

pub fn unescape(value: &str) -> Result<String, UnescapeError> {
    let mut result = String::new();
    let mut chars: Vec<char> = value.chars().rev().collect();

    while let Some(c) = chars.pop() {
        if c != '\\' {
            result.push(c);
            continue;
        }

        let c = match chars.pop() {
            Some(c) => c,
            None => return Err(UnescapeError::IncompleteStr { pos: result.len() }),
        };

        let c = match c {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\'' | '\"' | '\\' | '/' => c,
            _ => {
                return Err(UnescapeError::InvalidChar {
                    pos: result.len(),
                    char: c,
                });
            }
        };

        result.push(c);
    }

    return Ok(result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = unescape("hello\\nworld");
        assert_eq!(result, Ok("hello\nworld".to_string()));
    }

    #[test]
    fn it_works_with_invalid_char() {
        let result = unescape("hello\\\"world");
        assert_eq!(result, Ok("hello\"world".to_string()));
    }
}
