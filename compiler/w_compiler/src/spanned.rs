#[macro_export]
macro_rules! spanned {
    ($self: ident, $b: block) => {{
        let start = $self.start;
        let res = $b;
        (res, w_utils::span::Span::new(start, $self.end))
    }};
}
