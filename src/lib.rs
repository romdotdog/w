#![allow(clippy::must_use_candidate)]

pub mod diag;
pub mod lexer;
pub mod parser;
pub mod source;
pub mod span;

use appendlist::AppendList;
use diag::Message;
use lexer::Lexer;
use parser::Parser;
use source::Source;
use span::Span;

pub struct Session {
    sources: AppendList<Source>,
    warnings: AppendList<(Message, Span)>,
    pub errors: AppendList<(Message, Span)>,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceRef(usize);

/*
    TODO:
    * Create fork of AppendList
*/

impl Session {
    pub fn new() -> Self {
        Session {
            sources: AppendList::new(),
            warnings: AppendList::new(),
            errors: AppendList::new(),
        }
    }

    pub fn lexer(&self, src: SourceRef) -> Lexer<'_> {
        Lexer::new(self, src)
    }

    pub fn parse(&self, src: SourceRef) -> Parser<'_> {
        Parser::new(self, src)
    }

    pub fn register_source(&self, name: String, src: String) -> SourceRef {
        let r = self.sources.len();
        self.sources.push(Source::new(name, src));
        SourceRef(r)
    }

    pub fn get_source(&self, i: &SourceRef) -> &Source {
        &self.sources[i.0]
    }

    pub fn error(&self, msg: Message, span: Span) {
        self.errors.push((msg, span));
    }

    // TODO: Limit to non-wasm targets
    // Diagnostics will be handled on the JS side
    /// # Panics
    /// error span start (inclusive) >= error span end (exclusive)
    pub fn diagnostics(&self) {
        for i in 0..self.errors.len() {
            let (m, s) = self.errors.get(i).unwrap();
            let src = self.get_source(&s.src);
            let start_pos = s.start;
            let end_pos = s.end;

            assert!(start_pos < end_pos, "{} - {} / {}", start_pos, end_pos, m);

            let (line, column, (start_of_line, end_of_line)) = src.line_col(start_pos);
            let spaces = src.content()[start_of_line..start_pos]
                .chars()
                .map(|c| if c.is_whitespace() { c } else { ' ' })
                .collect::<String>();

            println!(
                "\x1b[1m{}:{}:{}: \x1b[91merror:\x1b[0m {}\n{}\n{}\x1b[91m\x1b[1m{}\x1b[0m",
                src.name(),
                line,
                column,
                m,
                src.content()[start_of_line..end_of_line].trim_end(),
                spaces,
                "^".repeat(s.end - start_pos),
            )
        }
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::Token;
    use super::Session;

    #[test]
    fn lexer() {
        let sess = Session::new();
        let src = sess.register_source("lexer_test".to_owned(), "a 1.2 b //".to_owned());
        let mut lex = sess.lexer(src);

        assert_eq!(lex.next(), Some(Token::Ident("a".to_owned())));
        assert_eq!(lex.next(), Some(Token::Float(1.2)));
        assert_eq!(lex.next(), Some(Token::Ident("b".to_owned())));
        assert_eq!(lex.next(), None);
    }
}
