use proc_macro::TokenStream;
use proc_macro2::{
    Group as Group2, Literal as Literal2, TokenStream as TokenStream2, TokenTree as TokenTree2,
};

use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitInt, Token,
};

// Define container for relevant state
#[allow(dead_code)]
#[derive(Debug)]
struct Seq {
    binding: Ident,
    start: LitInt,
    end: LitInt,
    body: TokenStream2,
}

impl Seq {
    fn transform(&self, tt: TokenTree2, i: usize) -> TokenTree2 {
        match tt {
            // If this `TokenTree` is a group, (recursively) expand the group so that all required
            // transformations may occur.
            TokenTree2::Group(g) => {
                let mut expanded = Group2::new(g.delimiter(), self.expand(g.stream(), i));
                expanded.set_span(g.span());
                TokenTree2::Group(expanded)
            }
            // If this `TokenTree` is the token we're looking for, transform it.
            TokenTree2::Ident(ref ident) if ident == &self.binding => {
                let mut lit = Literal2::usize_unsuffixed(i);
                lit.set_span(ident.span());
                TokenTree2::Literal(lit)
            }
            // Otherwise (`TokenTree` is neither a `Group` nor the `Literal` we are looking for),
            // pass it through.
            body => body,
        }
    }

    fn end_value(&self) -> usize {
        self.end
            .base10_parse()
            .expect("`end: LitInt` value could not be parsed as a (base 10) `usize`")
    }

    fn expand(&self, stream: TokenStream2, i: usize) -> TokenStream2 {
        stream.into_iter().map(|tt| self.transform(tt, i)).collect()
    }

    fn start_value(&self) -> usize {
        self.start
            .base10_parse()
            .expect("`start: LitInt` value could not be parsed as a (base 10) `usize`.")
    }
}

impl From<Seq> for TokenStream {
    fn from(seq: Seq) -> Self {
        (seq.start_value()..seq.end_value())
            .map(|i| TokenStream::from(seq.expand(seq.body.clone(), i)))
            .collect()
    }
}

// Define custom parser
impl Parse for Seq {
    // Invoked with `seq!(N in <start>..<end> { /* <user-defined body> });`
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let n = input.parse()?;
        let _in = input.parse::<Token![in]>()?;
        let start = input.parse()?;
        let _dot_dot = input.parse::<Token![..]>()?;
        let end = input.parse()?;
        // `braced!` requires an outparam
        let content;
        let _braces = braced!(content in input);
        let body = TokenStream2::parse(&content)?;

        Ok(Seq {
            binding: n,
            start,
            end,
            body,
        })
    }
}

// Transform the abstract syntax tree
#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Seq);
    input.into()
}
