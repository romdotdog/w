use w_ast::Span;
use w_errors::Message;

pub trait Handler {
    /// the parser will carry this so that the source will stay valid
    /// it is expected that the parser can get the source from this type
    /// using `get_source`
    type SourceRef;
    type LexerInput: Iterator<Item = char>;

    fn error(&self, src_ref: &Self::SourceRef, msg: Message, span: Span);
    fn load_source(&self, name: String) -> Option<Self::SourceRef>;
    fn get_source(&self, src_ref: &Self::SourceRef) -> Self::LexerInput;
}
