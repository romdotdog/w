use std::option::Option;
use w_ast::{Span, AST};
use w_errors::Message;

#[derive(Clone, Copy)]
pub enum Status {
    NotParsing,
    CurrentlyParsing,
    AlreadyParsed,
}

pub trait Handler<'ast> {
    type SourceRef;

    fn error(&self, src_ref: &'ast Self::SourceRef, msg: Message, span: Span);
    fn load_source(
        &'ast self,
        src_ref: &'ast Self::SourceRef,
        path: &'ast str,
    ) -> Option<(&'ast Self::SourceRef, Status)>;
    fn get_source(&self, src_ref: &'ast Self::SourceRef) -> &'ast str;
    fn set_ast(&self, src_ref: &'ast Self::SourceRef, prog: AST<'ast>);
}

// vv helpers vv

pub trait ImportlessHandler<'ast> {
    fn error(&self, msg: Message, span: Span);
}

pub struct ImportlessHandlerHandler<'ast, I: ImportlessHandler<'ast>> {
    pub handler: &'ast I,
}

impl<'ast, I: ImportlessHandler<'ast>> Handler<'ast> for ImportlessHandlerHandler<'ast, I> {
    type SourceRef = ();

    fn error(&self, _src_ref: &'ast Self::SourceRef, msg: Message, span: Span) {
        self.handler.error(msg, span);
    }

    fn load_source(
        &'ast self,
        _src_ref: &'ast Self::SourceRef,
        _path: &'ast str,
    ) -> Option<(&'ast (), Status)> {
        panic!("inappropriate use of ImportlessHandler");
    }

    fn get_source(&self, _src_ref: &'ast Self::SourceRef) -> &'ast str {
        panic!("inappropriate use of ImportlessHandler");
    }

    fn set_ast(&self, _src_ref: &'ast Self::SourceRef, _ast: AST<'ast>) {
        panic!("inappropriate use of ImportlessHandler");
    }
}
