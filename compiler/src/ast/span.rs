use std::io;

use owo_colors::{AnsiColors, OwoColorize};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextSpan {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct LineSpan {
    pub start: u32,
    pub end: u32,
}

fn get_line_index(text: &str, position: usize) -> usize {
    text[..position.min(text.len())]
        .bytes()
        .filter(|&b| b == b'\n')
        .count()
}

impl LineSpan {
    fn from_text_span(span: TextSpan, source: &str) -> Option<Self> {
        if !span.is_subset_of(TextSpan {
            start: 0,
            end: source.len() as u32,
        }) {
            return None;
        }

        let start = get_line_index(source, span.start as usize);
        let end = start
            + get_line_index(
                &source[span.start as usize..],
                (span.end - span.start) as usize,
            );

        Some(LineSpan {
            start: start as u32,
            end: end as u32,
        })
    }

    fn get_tolerant_start_trim_offset(&self, source: &str) -> usize {
        source
            .lines()
            .skip(self.start as usize)
            .take((self.end - self.start + 1) as usize)
            .filter_map(|line| line.find(|c: char| !c.is_whitespace()))
            .min()
            .unwrap_or(0)
    }
}

impl TextSpan {
    #[inline]
    pub fn new(start: u32, end: u32) -> Self {
        TextSpan { start, end }
    }

    pub fn from_line(source: &str, line_index: usize) -> Self {
        let start: usize = source
            .lines()
            .take(line_index)
            .map(|line| line.len() + 1)
            .sum();
        let end = source
            .lines()
            .nth(line_index)
            .map_or(start, |line| start + line.len());

        TextSpan {
            start: start as u32,
            end: end as u32,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    #[inline]
    pub fn get_text<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.start as usize..self.end as usize)
    }

    #[inline]
    pub fn is_subset_of(&self, other: TextSpan) -> bool {
        self.start >= other.start && self.end <= other.end
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    #[inline]
    pub fn clamp_to(&self, other: TextSpan) -> TextSpan {
        TextSpan {
            start: self.start.max(other.start),
            end: self.end.min(other.end),
        }
    }

    #[inline]
    pub fn combine(span1: TextSpan, span2: TextSpan) -> TextSpan {
        TextSpan {
            start: span1.start.min(span2.start),
            end: span1.end.max(span2.end),
        }
    }

    pub fn to_trimmed(&self, source: &str) -> Option<Self> {
        let text = self.get_text(source)?;
        let start_trim = text.find(|c: char| !c.is_whitespace()).unwrap_or(0);
        let end_trim = text
            .rfind(|c: char| !c.is_whitespace())
            .map_or(0, |idx| idx + 1);
        Some(TextSpan {
            start: self.start + start_trim as u32,
            end: self.start + end_trim as u32,
        })
    }
}

#[derive(Debug)]
pub enum SpanPrinterError {
    OutOfBoundsSpan,
    WriteError(io::Error),
}

impl std::fmt::Display for SpanPrinterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpanPrinterError::OutOfBoundsSpan => {
                write!(f, "span is out of bounds of the text")
            }
            SpanPrinterError::WriteError(e) => write!(f, "write error: {}", e),
        }
    }
}

impl std::error::Error for SpanPrinterError {}

pub trait SpanPrinter {
    fn print(&mut self, span: TextSpan, source: &str) -> Result<(), SpanPrinterError>;
}

pub struct SpanMultilinePrinter<W: io::Write> {
    stdout: W,
    trim: bool,
    highlight_color: AnsiColors,
}

impl Default for SpanMultilinePrinter<io::Stdout> {
    fn default() -> Self {
        Self {
            stdout: io::stdout(),
            trim: true,
            highlight_color: AnsiColors::Red,
        }
    }
}

impl<W: io::Write> SpanPrinter for SpanMultilinePrinter<W> {
    fn print(&mut self, span: TextSpan, source: &str) -> Result<(), SpanPrinterError> {
        if !span.is_subset_of(TextSpan::new(0, source.len() as u32)) {
            return Err(SpanPrinterError::OutOfBoundsSpan);
        }

        let processed_span = match self.trim {
            true => span.to_trimmed(source).expect("span within bounds"),
            false => span,
        };
        let lines_span =
            LineSpan::from_text_span(processed_span, source).expect("span within bounds");

        let start_trim_offset = match self.trim {
            true => lines_span.get_tolerant_start_trim_offset(source),
            false => 0,
        };
        let line_number_padding = lines_span.end.to_string().len();

        for line_index in lines_span.start..=lines_span.end {
            let raw_line_span = TextSpan::from_line(source, line_index as usize);

            let trimmed_end_len = match self.trim {
                true => raw_line_span.get_text(source).map_or(0, |line| {
                    line.rfind(|c: char| !c.is_whitespace())
                        .map_or(line.len(), |idx| idx + 1)
                }),
                false => raw_line_span.len(),
            };

            let line_span = TextSpan::new(
                raw_line_span.start + start_trim_offset as u32,
                raw_line_span.start + trimmed_end_len as u32,
            );

            if line_span.is_empty() {
                match writeln!(
                    &mut self.stdout,
                    "{:>line_number_padding$} {}",
                    (line_index + 1).to_string().blue(),
                    "|".blue()
                ) {
                    Ok(_) => continue,
                    Err(e) => return Err(SpanPrinterError::WriteError(e)),
                }
            }

            let underline_span = span.clamp_to(line_span);

            match writeln!(
                &mut self.stdout,
                "{:>line_number_padding$} {}    {}{}{}",
                (line_index + 1).to_string().blue().bold(),
                "|".blue().bold(),
                TextSpan::new(line_span.start, underline_span.start)
                    .get_text(source)
                    .unwrap_or(""),
                TextSpan::new(underline_span.start, underline_span.end)
                    .get_text(source)
                    .unwrap_or("")
                    .color(self.highlight_color)
                    .underline()
                    .bold(),
                TextSpan::new(underline_span.end, line_span.end)
                    .get_text(source)
                    .unwrap_or("")
            ) {
                Ok(_) => continue,
                Err(e) => return Err(SpanPrinterError::WriteError(e)),
            };
        }

        Ok(())
    }
}

pub struct SpanSingleLinePrinter<W: io::Write> {
    stdout: W,
    trim: bool,
    highlight_color: AnsiColors,
    highlight_symbol: char,
}

impl Default for SpanSingleLinePrinter<io::Stdout> {
    fn default() -> Self {
        Self {
            stdout: io::stdout(),
            trim: true,
            highlight_color: AnsiColors::Red,
            highlight_symbol: '^',
        }
    }
}

impl<W: io::Write> SpanPrinter for SpanSingleLinePrinter<W> {
    fn print(&mut self, span: TextSpan, source: &str) -> Result<(), SpanPrinterError> {
        if !span.is_subset_of(TextSpan::new(0, source.len() as u32)) {
            return Err(SpanPrinterError::OutOfBoundsSpan);
        }

        let trimmed_span = match self.trim {
            true => span.to_trimmed(source).expect("span within bounds"),
            false => span,
        };

        let line_index = get_line_index(source, trimmed_span.start as usize);
        let line_number = (line_index + 1).to_string();
        let line_number_padding = line_number.len();

        let line_span = TextSpan::from_line(source, line_index);
        let highlight_span = trimmed_span.clamp_to(line_span);
        let start_offset = (highlight_span.start - line_span.start) as usize;

        match writeln!(
            &mut self.stdout,
            "{} {}",
            " ".repeat(line_number_padding),
            "|".blue().bold(),
        ) {
            Ok(_) => {}
            Err(e) => return Err(SpanPrinterError::WriteError(e)),
        }
        match writeln!(
            &mut self.stdout,
            "{:>line_number_padding$} {}    {}",
            line_number.blue().bold(),
            "|".blue().bold(),
            line_span.get_text(source).unwrap_or("")
        ) {
            Ok(_) => {}
            Err(e) => return Err(SpanPrinterError::WriteError(e)),
        }
        match writeln!(
            &mut self.stdout,
            "{} {}    {}{}",
            " ".repeat(line_number_padding),
            "|".blue().bold(),
            " ".repeat(start_offset),
            self.highlight_symbol
                .to_string()
                .repeat(highlight_span.len())
                .color(self.highlight_color)
                .bold()
        ) {
            Ok(_) => {}
            Err(e) => return Err(SpanPrinterError::WriteError(e)),
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;

    #[test]
    fn test_line_span() {
        let source = indoc! {"
            def hello():
                print('Hello, world!')

            hello()
        "};

        let span = TextSpan { start: 15, end: 32 };
        let line_span = LineSpan::from_text_span(span, source).unwrap();
        assert_eq!(
            line_span.start,
            get_line_index(source, span.start as usize) as u32
        );
        assert_eq!(
            line_span.end,
            get_line_index(source, span.end as usize) as u32
        );
    }

    #[test]
    fn multi_line_test() {
        let source = indoc! {"
            function print() {
                console.log('Hello, world!')
            }
        "};

        let mut buffer = Vec::new();
        let mut printer = SpanMultilinePrinter {
            trim: false,
            highlight_color: AnsiColors::Green,
            stdout: &mut buffer,
        };

        let span = TextSpan { start: 17, end: 53 };
        let _ = printer.print(span, source);

        let output = String::from_utf8(buffer).expect("Invalid UTF-8 in output");
        println!("{}", output);
    }

    #[test]
    fn single_line_test() {
        let source = indoc! {"
            def hello():
                print('Hello, world!')

            hello()
        "};

        let mut buffer = Vec::new();
        let mut printer = SpanSingleLinePrinter {
            trim: false,
            highlight_color: AnsiColors::Green,
            stdout: &mut buffer,
            highlight_symbol: '-',
        };

        let span = TextSpan { start: 23, end: 38 };
        let _ = printer.print(span, source);

        let output = String::from_utf8(buffer).expect("Invalid UTF-8 in output");
        println!("{}", output);
    }
}
