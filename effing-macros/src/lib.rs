// we're already on nightly for generators, so we might as well :)
#![feature(let_else)]

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Error, Expr, ExprBreak, ExprReturn, GenericParam, Generics, Ident, ItemFn, LifetimeDef, Member,
    Pat, PatTupleStruct, PathArguments, ReturnType, Signature, Token, Type, TypeParam,
    TypePath, Visibility,
};

fn quote_do(e: &Expr) -> Expr {
    parse_quote! {
        {
            use ::core::ops::{Generator, GeneratorState};
            use ::effing_mad::frunk::Coproduct;
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
                        let injs = yield ::effing_mad::frunk::Coproduct::inject(effect);
                        let injs = ::effing_mad::macro_impl::get_inj(injs, marker2).unwrap();
                        ::effing_mad::macro_impl::get_inj2(injs, marker).unwrap()
                    }
                }
            }
            e => e,
        }
    }
}

/// Define an effectful function using `fn`-like syntax.
///
/// Effectful functions can suspend their execution, and when called immediately return an
/// effectful computation. This is analogous to `async fn`s, which can also suspend their
/// execution, and return a `Future` when called.
///
/// # Usage
/// ```rust
/// #[effectful(A, B)]
/// fn cool_function(arg: Foo) -> Bar {
///     yield expr_a;
///     let val = yield expr_b;
///     epic_function(val).do_
/// }
/// ```
/// This macro takes a list of types as its arguments. These types must implement
/// (`Effect`)[effing_mad::Effect]. Then, `expr_a` and `expr_b` must each be some type that
/// implements either `IntoEffect<Effect = A>` or `IntoEffect<Effect = B>`.
///
/// The `yield expr` syntax runs the effect `expr`, and evaluates to the Injection of that
/// [`IntoEffect`] type.
///
/// The `do_` operator is analogous to `.await`. It runs an effectful computation by yielding all
/// of its effects to the caller. The callee's effects must be a subset of the caller's effects -
/// in this example, a subset of `{A, B}`. `epic_function` is usually another function defined
/// using this macro.
#[proc_macro_attribute]
pub fn effectful(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut effects = parse_macro_input!(args as Effectful);
    let effect_names = effects.effects.iter();
    let yield_type = quote! {
        <::effing_mad::frunk::Coprod!(#(#effect_names),*) as ::effing_mad::macro_impl::FlattenEffects>::Out
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
            <#yield_type as ::effing_mad::injection::EffectList>::Injections,
            Yield = #yield_type,
            Return = #return_type
        > {
            move |_begin: <#yield_type as ::effing_mad::injection::EffectList>::Injections| {
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

        Ok(Effect {
            name,
            args,
            ret,
        })
    }
}

struct Effects {
    vis: Visibility,
    group_name: Ident,
    generics: Generics,
    effects: Punctuated<Effect, Token![;]>,
}

impl Parse for Effects {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let vis = input.parse()?;
        let group_name = input.parse()?;
        let generics = input.parse()?;

        let content;
        braced!(content in input);
        let effects = Punctuated::parse_terminated(&content)?;

        Ok(Effects {
            vis,
            group_name,
            generics,
            effects,
        })
    }
}

/// Define a new effect type, by multiplexing sub-effects into one type.
///
/// # Usage
/// ```rust
/// effects! {
///     state::State<T> {
///         fn get() -> T;
///         fn put(v: T) -> ();
///     }
/// }
/// ```
/// It is necessary to provide a unique module name (prefer the snake_case_version of the type name)
/// because of Horrible Macro Reasons. This example produces a new module called `state` with the
/// `State` effect type inside it.
///
/// This allows usage such as `let state = yield State::get()` and `yield State::put(val)` - the
/// type after `->` is the injection that that sub-effect has. This is why the `IntoEffect` trait
/// exists; the injection of `get()` is a single enum variant of the injection of `State<T>`. The
/// former is what a user is interested in, but due to multiplexing, the latter is what gets passed
/// into a computation when it is resumed.
#[proc_macro]
pub fn effects(input: TokenStream) -> TokenStream {
    let Effects {
        vis,
        group_name,
        generics,
        effects,
    } = parse_macro_input!(input as Effects);

    let eff_name = effects.iter().map(|eff| &eff.name).collect::<Vec<_>>();
    let phantom_datas = generics
        .params
        .iter()
        .map(|param| match param {
            GenericParam::Type(TypeParam { ident, .. }) => {
                quote!(::core::marker::PhantomData::<#ident>)
            }
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => {
                quote!(::core::marker::PhantomData::<&#lifetime ()>)
            }
            syn::GenericParam::Const(_) => todo!(),
        })
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
        #vis struct #group_name #generics (#phantom_datas);

        impl #generics #group_name #generics {
            #(
            fn #eff_name(#(#arg_name: #arg_ty),*) -> #eff_name #generics {
                #eff_name(#(#arg_name,)* #phantom_datas)
            }
            )*
        }

        impl #generics ::effing_mad::EffectGroup for #group_name #generics {
            type Effects = ::effing_mad::frunk::Coprod!(#(#eff_name #generics),*);
        }

        #(
        #[allow(non_camel_case_types)]
        #vis struct #eff_name #generics (#(#arg_ty,)* #phantom_datas);

        impl #generics ::effing_mad::Effect for #eff_name #generics {
            type Injection = #ret_ty;
        }
        )*
    }
    .into()
}

struct HandlerArm {
    eff: Pat,
    body: Expr,
}

impl Parse for HandlerArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let eff = input.parse()?;
        <Token![=>]>::parse(input)?;
        let body = input.parse()?;
        Ok(HandlerArm { eff, body })
    }
}

struct Handler {
    asyncness: Option<Token![async]>,
    moveness: Option<Token![move]>,
    group: TypePath,
    arms: Punctuated<HandlerArm, Token![,]>,
}

impl Parse for Handler {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let asyncness = input.parse()?;
        let moveness = input.parse()?;
        let group = input.parse()?;

        let content;
        braced!(content in input);
        let arms = Punctuated::parse_terminated(&content)?;
        Ok(Handler {
            asyncness,
            moveness,
            group,
            arms,
        })
    }
}

// can't figure out what the type is lol
struct FixControlFlow<T: ToTokens>(T);
impl<T: ToTokens> syn::fold::Fold for FixControlFlow<T> {
    fn fold_expr(&mut self, e: Expr) -> Expr {
        let eff = &self.0;
        match e {
            Expr::Break(ExprBreak { expr, .. }) => {
                let expr = expr.as_ref().map(ToTokens::to_token_stream).unwrap_or(quote!(()));
                parse_quote!(return ::core::ops::ControlFlow::Break(#expr))
            },
            Expr::Return(ExprReturn { expr, .. }) => {
                let expr = expr.as_ref().map(ToTokens::to_token_stream).unwrap_or(quote!(()));
                parse_quote! {
                    return ::core::ops::ControlFlow::Continue(
                        ::effing_mad::frunk::Coproduct::inject(
                            ::effing_mad::injection::Tagged::<_, #eff>::new(#expr)
                        )
                    )
                }
            },
            e => e,
        }
    }
}

/// Define a handler for an effect type whose definition uses [`effects!`]
///
/// # Usage
/// ```rust
/// let mut state = 0i32;
/// handler! {
///     state::State<i32>,
///     get() => ControlFlow::Continue(state),
///     put(v) => {
///         state = v;
///         ControlFlow::Continue(())
///     },
/// }
/// ```
/// The handler can capture state from its environment, and/or be asynchronous. The keywords
/// `async` and `move` can both optionally appear (in that order) at the very beginning of the
/// macro input to control these behaviours, in a similar way to how they would affect a closure.
///
/// It is necessary to provide both the module (`snake_case`) and type (`PascalCase`) names, due to
/// the same Horrible Macro Reasons as mentioned in the documentation of `effects!`. The sub-effects
/// (here `get` and `put`) here are demultiplexed into their respective handler arms upon
/// invocation of the handler. All sub-effects in an effect type must be handled in a given
/// invocation of this macro.
///
/// Notice how the `put` arm in this example mutably borrows the `state` variable, while the `get`
/// arm also borrows it. This is the advantage of multiplexing effects. Internally, `handler!`
/// expands to a single closure with a `match` expression in it, so the arms can all borrow the
/// same content, even mutably.
#[proc_macro]
pub fn handler(input: TokenStream) -> TokenStream {
    let Handler {
        asyncness,
        moveness,
        group,
        arms,
    } = parse_macro_input!(input as Handler);

    let PathArguments::AngleBracketed(ref generics) = group.path.segments.last().unwrap().arguments else { panic!("agh") };

    let mut matcher = quote! { match effs {} };
    for arm in arms {
        let HandlerArm { eff, mut body } = arm;
        let eff_ty = match &eff {
            // struct name on its own gets parsed as ident
            Pat::Ident(ident) => quote!(#ident),
            Pat::Path(path) => quote!(#path),
            Pat::TupleStruct(PatTupleStruct { path, .. }) => quote!(#path),
            p => panic!("invalid pattern in handler: {p:?}"),
        };
        if let Expr::Break(_) | Expr::Return(_) = body {
            body = parse_quote!({ #body });
        }
        body = syn::fold::fold_expr(&mut FixControlFlow(&eff_ty), body.clone());
        matcher = quote! {
            match effs.uninject() {
                Ok(#eff) => {
                    let __effing_inj = #body;
                    #[allow(unreachable_code)]
                    ::effing_mad::frunk::Coproduct::inject(
                        ::effing_mad::injection::Tagged::<_, #eff_ty #generics>::new(__effing_inj)
                    )
                },
                Err(effs) => #matcher,
            }
        };
    }
    let effs_ty = quote!(<#group as ::effing_mad::EffectGroup>::Effects);
    let injs_ty = quote!(<#effs_ty as ::effing_mad::injection::EffectList>::Injections);
    let ret_ty = quote!(::core::ops::ControlFlow<_, #injs_ty>);
    quote! {
        #moveness |effs: #effs_ty| -> #ret_ty #asyncness {
            let __effing_inj = #matcher;
            // if the handler unconditionally breaks then this line is unreachable, but we
            // don't want to see a warning for it.
            #[allow(unreachable_code)]
            ::core::ops::ControlFlow::<_, _>::Continue(__effing_inj)
        }
    }
    .into()
}
