use w_errors::Message;
use w_lexer::Span;

pub trait Handler {
    /// the parser will carry this so that the source will stay valid
    /// it is expected that the parser can get the source from this type
    /// using `get_source`
    type SourceRef;

    fn error(&self, src_ref: &Self::SourceRef, msg: Message, span: Span);
    fn load_source(&self, name: String) -> Option<Self::SourceRef>;
    fn get_source<'a>(&'a self, src_ref: &'a Self::SourceRef) -> &'a str;
}
