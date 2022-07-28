// we're already on nightly for generators, so we might as well :)
#![feature(let_else)]

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Error, Expr, Ident, ItemFn, Member, ReturnType, Signature, Token,
};

fn quote_do(e: &Expr) -> Expr {
    parse_quote! {
        let gen = #e;
        let pinned = ::core::pin::Pin::new(&mut gen);
        loop {
            match gen.resume(()) {
                ::core::ops::GeneratorState::Yielded(effs) => yield effs.embed(),
                ::core::ops::GeneratorState::Complete(v) => break v,
            }
        }
    }
}

struct Effects {
    effects: Vec<Ident>,
}

impl Parse for Effects {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let effects = Punctuated::<Ident, Token![,]>::parse_terminated(input)?;
        Ok(Effects {
            effects: effects.into_iter().collect(),
        })
    }
}

impl syn::fold::Fold for Effects {
    fn fold_expr(&mut self, e: Expr) -> Expr {
        match e {
            Expr::Field(ref ef) => {
                let Member::Named(ref name) = ef.member else { return e };
                if name == "do" {
                    quote_do(&ef.base)
                } else {
                    e
                }
            }
            Expr::Yield(ref y) => {
                let Some(ref expr) = y.expr else { panic!("no expr?") };
                parse_quote! {
                    yield ::frunk::Coproduct::inject(#expr)
                }
            }
            e => e,
        }
    }
}

#[proc_macro_attribute]
pub fn effectful(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut effects = parse_macro_input!(args as Effects);
    let effect_names = &effects.effects;
    let yield_type = quote! {
        ::frunk::Coprod!(#(#effect_names),*)
    };
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = parse_macro_input!(item as ItemFn);
    let Signature {
        constness,
        unsafety,
        ident,
        generics,
        inputs,
        output,
        ..
    } = sig;
    let return_type = match output {
        ReturnType::Default => quote!(()),
        ReturnType::Type(_r_arrow, ref ty) => ty.to_token_stream(),
    };
    let new_block = syn::fold::fold_block(&mut effects, *block);
    quote! {
        #(#attrs)*
        #vis #constness #unsafety
        fn #ident #generics(#inputs) -> impl ::core::ops::Generator<Yield = #yield_type, Return = #return_type> {
            move || {
                #new_block
            }
        }
    }.into()
}
