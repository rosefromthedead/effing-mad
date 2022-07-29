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
        {
            use ::core::ops::{Generator, GeneratorState};
            use ::frunk::coproduct::Coproduct;
            let mut gen = #e;
            let mut injection = Coproduct::inject(::effing_mad::injection::Begin);
            loop {
                // safety: same as in `handle`
                let pinned = unsafe { ::core::pin::Pin::new_unchecked(&mut gen) };
                match pinned.resume(injection) {
                    GeneratorState::Yielded(effs) =>
                        injection = (yield effs.embed()).subset().ok().unwrap(),
                    GeneratorState::Complete(v) => break v,
                }
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
                if name == "do_" {
                    quote_do(&ef.base)
                } else {
                    e
                }
            }
            Expr::Yield(ref y) => {
                let Some(ref expr) = y.expr else { panic!("no expr?") };
                parse_quote! {
                    {
                        let effect = { #expr };
                        let marker = ::effing_mad::macro_impl::mark(&effect);
                        let injs = yield ::frunk::coproduct::Coproduct::inject(effect);
                        ::effing_mad::macro_impl::get_inj(injs, marker).unwrap()
                    }
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
        fn #ident #generics(#inputs)
        -> impl ::core::ops::Generator<
            <#yield_type as ::effing_mad::injection::InjectionList>::List,
            Yield = #yield_type,
            Return = #return_type
        > {
            move |_begin: <#yield_type as ::effing_mad::injection::InjectionList>::List| {
                #new_block
            }
        }
    }
    .into()
}
