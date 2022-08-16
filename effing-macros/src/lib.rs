// we're already on nightly for generators, so we might as well :)
#![feature(let_else)]

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Error, Expr, GenericParam, Generics, Ident, ItemFn, LifetimeDef, Member, Pat, ReturnType,
    Signature, Token, Type, TypeParam,
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

struct Effectful {
    effects: Punctuated<Type, Token![,]>,
}

impl Parse for Effectful {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let effects = Punctuated::parse_terminated(input)?;
        Ok(Effectful { effects })
    }
}

impl syn::fold::Fold for Effectful {
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
                        let into_effect = { #expr };
                        let marker = ::effing_mad::macro_impl::mark(&into_effect);
                        let effect = ::effing_mad::IntoEffect::into_effect(into_effect);
                        let marker2 = ::effing_mad::macro_impl::mark(&effect);
                        let injs = yield ::frunk::coproduct::Coproduct::inject(effect);
                        let injs = ::effing_mad::macro_impl::get_inj(injs, marker2).unwrap();
                        ::effing_mad::macro_impl::get_inj2(injs, marker).unwrap()
                    }
                }
            }
            e => e,
        }
    }
}

#[proc_macro_attribute]
pub fn effectful(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut effects = parse_macro_input!(args as Effectful);
    let effect_names = &effects.effects;
    let mut yield_type = quote! {
        ::frunk::coproduct::CNil
    };
    for effect in effect_names {
        yield_type = quote! {
            <#effect as ::effing_mad::macro_impl::EffectSet<#yield_type>>::Out
        };
    }
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

struct EffectArg {
    name: Ident,
    ty: Type,
}

impl Parse for EffectArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let _: Token![:] = input.parse()?;
        let ty: Type = input.parse()?;
        Ok(EffectArg { name, ty })
    }
}

struct Effect {
    name: Ident,
    args: Vec<EffectArg>,
    ret: Type,
}

impl Parse for Effect {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        <Token![fn]>::parse(input)?;
        let name = input.parse()?;

        let content;
        parenthesized!(content in input);
        let args = Punctuated::<EffectArg, Token![,]>::parse_terminated(&content)?;
        let args = args.into_iter().collect();

        <Token![->]>::parse(input)?;
        let ret = input.parse()?;

        Ok(Effect { name, args, ret })
    }
}

struct Effects {
    name: Ident,
    generics: Generics,
    effects: Punctuated<Effect, Token![;]>,
}

impl Parse for Effects {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let generics = input.parse()?;

        let content;
        braced!(content in input);
        let effects = Punctuated::parse_terminated(&content)?;

        Ok(Effects {
            name,
            generics,
            effects,
        })
    }
}

#[proc_macro]
pub fn effects(input: TokenStream) -> TokenStream {
    let Effects {
        name,
        generics,
        effects,
    } = parse_macro_input!(input as Effects);
    let injs_name = Ident::new(&format!("{}Injs", name.to_string()), Span::call_site());

    let variants = effects
        .iter()
        .map(|Effect { name, .. }| format_ident!("__{name}"))
        .collect::<Vec<_>>();
    let structs = effects
        .iter()
        .map(|Effect { name, .. }| format_ident!("__{name}"))
        .collect::<Vec<_>>();
    let eff_name = effects.iter().map(|eff| &eff.name).collect::<Vec<_>>();
    let phantom_data_tys = generics
        .params
        .iter()
        .map(|param| match param {
            GenericParam::Type(TypeParam { ident, .. }) => {
                quote!(::core::marker::PhantomData<#ident>)
            }
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => {
                quote!(::core::marker::PhantomData<&#lifetime ()>)
            }
            syn::GenericParam::Const(_) => todo!(),
        })
        .collect::<Vec<_>>();
    let phantom_data_tys = quote!(#(#phantom_data_tys),*);
    let phantom_datas = generics
        .params
        .iter()
        .map(|_| quote!(::core::marker::PhantomData))
        .collect::<Vec<_>>();
    let phantom_datas = quote!(#(#phantom_datas),*);

    let arg_name = effects
        .iter()
        .map(|eff| eff.args.iter().map(|arg| &arg.name).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let arg_ty = effects
        .iter()
        .map(|eff| eff.args.iter().map(|arg| &arg.ty).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let ret_ty = effects.iter().map(|eff| &eff.ret).collect::<Vec<_>>();

    quote! {
        enum #name #generics {
            #(
            #variants(#(#arg_ty),*)
            ),*
        }

        enum #injs_name #generics {
            #(
            #variants(#ret_ty)
            ),*
        }

        impl #generics #name #generics {
            #(
            pub fn #eff_name(#(#arg_name: #arg_ty),*) -> #structs #generics {
                #structs(#(#arg_name,)* #phantom_datas)
            }
            )*
        }

        impl #generics ::effing_mad::Effect for #name #generics {
            type Injection = #injs_name #generics;
        }

        #(
        struct #structs #generics(#(#arg_ty,)* #phantom_data_tys);

        impl #generics ::effing_mad::IntoEffect for #structs #generics {
            type Effect = #name #generics;
            type Injection = #ret_ty;

            fn into_effect(self) -> Self::Effect {
                let #structs(#(#arg_name,)* ..) = self;
                #name::#variants(#(#arg_name),*)
            }
            fn inject(inj: #ret_ty) -> #injs_name #generics {
                #injs_name::#variants(inj)
            }
            fn uninject(injs: #injs_name #generics) -> Option<#ret_ty> {
                match injs {
                    #injs_name::#variants(inj) => Some(inj),
                    _ => None,
                }
            }
        }
        )*
    }
    .into()
}

struct HandlerArm {
    eff: Ident,
    args: Punctuated<Pat, Token![,]>,
    breaker: Expr,
}

impl Parse for HandlerArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let eff = input.parse()?;

        let content;
        parenthesized!(content in input);
        let args = Punctuated::parse_terminated(&content)?;

        <Token![=>]>::parse(input)?;
        let breaker = input.parse()?;

        Ok(HandlerArm { eff, args, breaker })
    }
}

struct Handler {
    r#async: Option<Token![async]>,
    r#move: Option<Token![move]>,
    eff: Ident,
    generics: Generics,
    arms: Punctuated<HandlerArm, Token![,]>,
}

impl Parse for Handler {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let r#async = input.parse()?;
        let r#move = input.parse()?;
        let eff = input.parse()?;
        let generics = input.parse()?;
        <Token![,]>::parse(input)?;
        let arms = Punctuated::parse_terminated(input)?;
        Ok(Handler {
            r#async,
            r#move,
            eff,
            generics,
            arms,
        })
    }
}

#[proc_macro]
pub fn handler(input: TokenStream) -> TokenStream {
    let handler = parse_macro_input!(input as Handler);
    let handler_async = handler.r#async.map(|_| quote!(async move));
    let handler_move = handler.r#move;
    let eff_name = handler.eff;
    let eff_generics = handler.generics;
    let injs_name = format_ident!("{eff_name}Injs");
    let eff = handler
        .arms
        .iter()
        .map(|HandlerArm { eff, .. }| format_ident!("__{eff}"));
    let arg_name = handler
        .arms
        .iter()
        .map(|HandlerArm { args, .. }| args.iter().collect::<Vec<_>>());
    let breaker = handler
        .arms
        .iter()
        .map(|HandlerArm { breaker, .. }| breaker);
    quote! {
        #handler_move |eff: #eff_name #eff_generics| #handler_async {
            match eff {
                #(
                #eff_name::#eff(#(#arg_name),*) => match #breaker {
                    ::core::ops::ControlFlow::Continue(inj) =>
                        ::core::ops::ControlFlow::Continue(#injs_name::#eff(inj)),
                    ::core::ops::ControlFlow::Break(ret) => ::core::ops::ControlFlow::Break(ret),
                }
                ),*
            }
        }
    }
    .into()
}
