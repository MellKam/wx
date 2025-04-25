use owo_colors::OwoColorize;

pub struct AstPrinter {
    indent: usize,
    pub result: String,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            result: String::new(),
        }
    }

    fn add_keyword(&mut self, keyword: &str) {
        self.result.push_str(keyword);
        self.result.push(' ');
    }
}
