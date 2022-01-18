use w_ast::Span;
use w_errors::Message;

pub trait Handler<'ast> {
    type SourceRef;

    fn error(&self, src_ref: &'ast Self::SourceRef, msg: Message, span: Span);
    fn load_source(&'ast self, name: String) -> Option<&'ast Self::SourceRef>;
    fn get_source(&self, src_ref: &'ast Self::SourceRef) -> &'ast [u8];
}
