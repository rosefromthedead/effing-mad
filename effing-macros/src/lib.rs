use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Error, Expr, ExprBreak, ExprReturn, GenericParam, Generics, Ident, ItemFn, LifetimeDef, Member,
    Pat, PatIdent, PatPath, PatTupleStruct, Path, PathArguments, PathSegment, ReturnType,
    Signature, Token, Type, TypeParam, TypePath, Visibility,
};

fn quote_do(e: &Expr) -> Expr {
    parse_quote! {
        {
            use ::core::ops::{Coroutine, CoroutineState};
            use ::effing_mad::frunk::Coproduct;
            let mut gen = #e;
            let mut injection = Coproduct::inject(::effing_mad::injection::Begin);
            loop {
                // interesting hack to trick the borrow checker
                // allows cloneable coroutines
                let res = {
                    // safety: same as in `handle_group`
                    let pinned = unsafe { ::core::pin::Pin::new_unchecked(&mut gen) };
                    pinned.resume(injection)
                };
                match res {
                    CoroutineState::Yielded(effs) =>
                        injection = (yield effs.embed()).subset().ok().unwrap(),
                    CoroutineState::Complete(v) => break v,
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

impl syn::visit_mut::VisitMut for Effectful {
    fn visit_expr_mut(&mut self, e: &mut Expr) {
        match e {
            Expr::Field(ref mut ef) => {
                self.visit_expr_mut(&mut ef.base);
                let Member::Named(ref name) = ef.member else {
                    return;
                };
                if name == "do_" {
                    *e = quote_do(&ef.base);
                }
            },
            Expr::Yield(ref y) => {
                let Some(ref expr) = y.expr else {
                    panic!("no expr?")
                };
                *e = parse_quote! {
                    {
                        let effect = { #expr };
                        let marker = ::effing_mad::macro_impl::mark(&effect);
                        let injs = yield ::effing_mad::frunk::Coproduct::inject(effect);
                        ::effing_mad::macro_impl::get_inj(injs, marker).unwrap()
                    }
                };
            },
            e => syn::visit_mut::visit_expr_mut(self, e),
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
/// ```rust,ignore
/// #[effectful(A, B)]
/// /* optionally: */ #[effectful::cloneable]
/// fn cool_function(arg: Foo) -> Bar {
///     yield expr_a;
///     let val = yield expr_b;
///     epic_function(val).do_
/// }
/// ```
/// This macro takes a list of types as its arguments. These types must implement `Effect` or
/// `EffectGroup`. Expressions passed to `yield` must be of one of those effect types, or of an
/// effect type that is in one of those groups.
///
/// The `yield expr` syntax runs the effect `expr`, and evaluates to the Injection of that effect
/// type.
///
/// The `do_` operator is analogous to `.await`. It runs an effectful computation by yielding all
/// of its effects to the caller of the current function. The callee's effects must be a subset of
/// the current function's effects - in this example, a subset of `{A, B}`. The callee is usually
/// another function defined using this macro.
///
/// It is possible to create effectful functions whose computations can be cloned. This requires
/// marking the function as `#[effectful::cloneable]` after the `#[effectful(...)]` invocation,
/// the function to have only `Clone` and `Unpin` locals, and the function to never hold a
/// reference to a local across a yield point. In other words, the underlying coroutine must be
/// `Clone` and `Unpin`.
#[proc_macro_attribute]
pub fn effectful(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut effects = parse_macro_input!(args as Effectful);
    let effect_names = effects.effects.iter();
    let yield_type = quote! {
        <::effing_mad::frunk::Coprod!(#(#effect_names),*) as ::effing_mad::macro_impl::FlattenEffects>::Out
    };
    let ItemFn {
        mut attrs,
        vis,
        sig,
        mut block,
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
    syn::visit_mut::visit_block_mut(&mut effects, &mut block);
    let mut cloneable = false;
    attrs.retain(|attr| {
        if attr.path == parse_quote!(effectful::cloneable) {
            cloneable = true;
            false // remove it from the attrs list so no one gets confused
        } else {
            true
        }
    });
    let clone_bound = cloneable.then_some(quote!( + ::core::clone::Clone + ::core::marker::Unpin));
    quote! {
        #(#attrs)*
        #vis #constness #unsafety
        fn #ident #generics(#inputs)
        -> impl ::core::ops::Coroutine<
            <#yield_type as ::effing_mad::injection::EffectList>::Injections,
            Yield = #yield_type,
            Return = #return_type
        > #clone_bound {
            move |_begin: <#yield_type as ::effing_mad::injection::EffectList>::Injections| {
                #block
            }
        }
    }
    .into()
}

struct EffectArg {
    vis: Visibility,
    name: Ident,
    ty: Type,
}

impl Parse for EffectArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let vis = input.parse()?;
        let name = input.parse()?;
        let _: Token![:] = input.parse()?;
        let ty: Type = input.parse()?;
        Ok(EffectArg { vis, name, ty })
    }
}

struct Effect {
    vis: Visibility,
    name: Ident,
    args: Punctuated<EffectArg, Token![,]>,
    ret: Type,
}

impl Parse for Effect {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let vis = input.parse()?;
        <Token![fn]>::parse(input)?;
        let name = input.parse()?;

        let content;
        parenthesized!(content in input);
        let args = Punctuated::parse_terminated(&content)?;

        <Token![->]>::parse(input)?;
        let ret = input.parse()?;

        Ok(Effect {
            vis,
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

/// Define a new group of effects.
///
/// # Usage
/// ```rust,ignore
/// effects! {
///     State<T> {
///         fn get() -> T;
///         fn put(v: T) -> ();
///     }
/// }
/// ```
///
/// Multiple new types are created: one for the group (`State` in this example) and one for each
/// effect in the group. The group type has associated functions that look like the functions in the
/// invocation.
///
/// This allows usage such as `let state = yield State::get()` and `yield State::put(val)`. The
/// type after `->` defines the injection type of that effect, which is the type that such yield
/// expressions will evaluate to.
#[proc_macro]
pub fn effects(input: TokenStream) -> TokenStream {
    let Effects {
        vis,
        group_name,
        generics,
        effects,
    } = parse_macro_input!(input as Effects);

    let (eff_vis, eff_name): (Vec<_>, Vec<_>) =
        effects.iter().map(|eff| (&eff.vis, &eff.name)).unzip();
    let phantom_data_tys = generics
        .params
        .iter()
        .map(|param| match param {
            GenericParam::Type(TypeParam { ident, .. }) => {
                parse_quote!(::core::marker::PhantomData::<#ident>)
            },
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => {
                parse_quote!(::core::marker::PhantomData::<&#lifetime ()>)
            },
            GenericParam::Const(_) => todo!(),
        })
        .collect::<Vec<Type>>();
    let group_phantom_data_field = generics
        .lt_token
        .map(|_| quote!(pub ::core::marker::PhantomData::<#group_name #generics>));
    let group_phantom_data = generics
        .lt_token
        .map(|_| quote!(::core::marker::PhantomData));

    let (arg_vis, (arg_name, arg_ty)): (Vec<_>, (Vec<_>, Vec<_>)) = effects
        .iter()
        .map(|eff| {
            let (arg_vis, (arg_name, arg_ty)): (Vec<_>, (Vec<_>, Vec<_>)) = eff
                .args
                .iter()
                .map(|arg| (&arg.vis, (&arg.name, &arg.ty)))
                .unzip();
            (arg_vis, (arg_name, arg_ty))
        })
        .unzip();
    let ret_ty = effects.iter().map(|eff| &eff.ret).collect::<Vec<_>>();

    quote! {
        #vis struct #group_name #generics (#(#phantom_data_tys),*);

        impl #generics #group_name #generics {
            #(
            #eff_vis fn #eff_name(#(#arg_name: #arg_ty),*) -> #eff_name #generics {
                #eff_name(#(#arg_name,)* #group_phantom_data)
            }
            )*
        }

        impl #generics ::effing_mad::EffectGroup for #group_name #generics {
            type Effects = ::effing_mad::frunk::Coprod!(#(#eff_name #generics),*);
        }

        #(
        #[allow(non_camel_case_types)]
        #vis struct #eff_name #generics (#(#arg_vis #arg_ty,)* #group_phantom_data_field);

        impl #generics ::effing_mad::Effect for #eff_name #generics {
            type Injection = #ret_ty;
        }
        )*
    }
    .into()
}

struct HandlerArm {
    pat: Pat,
    body: Expr,
}

impl Parse for HandlerArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pat = input.parse()?;
        <Token![=>]>::parse(input)?;
        let body = input.parse()?;
        Ok(HandlerArm { pat, body })
    }
}

struct Handler {
    asyncness: Option<Token![async]>,
    moveness: Option<Token![move]>,
    group: TypePath,
    arms: Punctuated<HandlerArm, Token![,]>,
    is_shorthand: bool,
}

impl Parse for Handler {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let asyncness = input.parse()?;
        let moveness = input.parse()?;
        let ahead = input.fork();
        if ahead.parse::<HandlerArm>().is_ok() {
            // the docs say not to do this but idk how to do it otherwise
            let single_arm: HandlerArm = input.parse().unwrap();
            let path = match &single_arm.pat {
                // struct name on its own gets parsed as ident
                Pat::Ident(PatIdent { ident, .. }) => Path {
                    leading_colon: None,
                    segments: Punctuated::from_iter(std::iter::once(PathSegment {
                        ident: ident.clone(),
                        arguments: PathArguments::None,
                    })),
                },
                Pat::Path(PatPath { path, .. }) | Pat::TupleStruct(PatTupleStruct { path, .. }) => {
                    path.clone()
                },
                p => panic!("invalid pattern in handler: {p:?}"),
            };
            let group = TypePath { qself: None, path };

            return Ok(Handler {
                asyncness,
                moveness,
                group,
                arms: Punctuated::from_iter(std::iter::once(single_arm)),
                is_shorthand: true,
            });
        }
        let group = input.parse()?;

        let content;
        braced!(content in input);
        let arms = Punctuated::parse_terminated(&content)?;
        Ok(Handler {
            asyncness,
            moveness,
            group,
            arms,
            is_shorthand: false,
        })
    }
}

// can't figure out what the type is lol
struct FixControlFlow<T: ToTokens> {
    eff_ty: T,
    is_shorthand: bool,
}

impl<T: ToTokens> syn::visit_mut::VisitMut for FixControlFlow<T> {
    fn visit_expr_mut(&mut self, e: &mut Expr) {
        let eff = &self.eff_ty;
        match e {
            Expr::Break(ExprBreak { expr, .. }) => {
                let expr = expr
                    .as_ref()
                    .map(ToTokens::to_token_stream)
                    .unwrap_or(quote!(()));
                *e = parse_quote!(return ::core::ops::ControlFlow::Break(#expr));
            },
            Expr::Return(ExprReturn { expr, .. }) => {
                let expr = expr
                    .as_ref()
                    .map(ToTokens::to_token_stream)
                    .unwrap_or(quote!(()));
                let inj = if self.is_shorthand {
                    quote!(#expr)
                } else {
                    quote!(::effing_mad::injection::Tagged::<_, #eff>::new(#expr))
                };
                *e = parse_quote! {
                    return ::core::ops::ControlFlow::Continue(
                        ::effing_mad::frunk::Coproduct::inject(#inj)
                    );
                };
            },
            e => syn::visit_mut::visit_expr_mut(self, e),
        }
    }
}

/// Define a handler for an effect or group of effects.
///
/// # Usage
/// Handling a group of effects at once:
/// ```rust,ignore
/// let mut state = 0i32;
/// handle_group(
///     g,
///     handler! {
///         State<i32> {
///             get() => state,
///             put(v) => state = v,
///         }
///     }
/// )
/// ```
///
/// Handling a single effect at once:
/// ```rust,ignore
/// handle(g, handler!(Cancel => break))
/// ```
///
/// The value that a handler arm's expression evaluates to (for example `state` and `()` in the
/// `State<i32>` example) is used as the injection for that effect. It is also possible to use the
/// `break` keyword to cause the computation that is being handled to return. In this case, the
/// type of the value passed to `break` must match the return type of a computation that the
/// handler is used on. See `effing_mad::map` for a way to change the return value of a computation.
///
/// The handler can capture state from its environment, and/or be asynchronous. The keywords
/// `async` and `move` can both optionally appear (in that order) at the very beginning of the
/// macro input to control these behaviours, in a similar way to how they would affect a closure.
/// These keywords apply to all arms of the handler. `handle_async` or `handle_group_async` must be
/// used when applying an async handler.
///
/// Note that the `put` arm in this example mutably borrows the `state` variable, while the `get`
/// arm also borrows it. This is the advantage of handling effects together. Internally, `handler!`
/// expands to a single closure with a `match` expression in it, so the arms can all borrow the
/// same content, even mutably.
#[proc_macro]
pub fn handler(input: TokenStream) -> TokenStream {
    let Handler {
        asyncness,
        moveness,
        group,
        arms,
        is_shorthand,
    } = parse_macro_input!(input as Handler);

    let generics = match group.path.segments.last().unwrap().arguments {
        PathArguments::None => None,
        PathArguments::AngleBracketed(ref v) => Some(v),
        PathArguments::Parenthesized(_) => panic!("stop that"),
    };

    let mut matcher = quote! { match effs {} };
    for arm in arms {
        let HandlerArm { pat, mut body } = arm;
        let eff_ty = match &pat {
            // struct name on its own gets parsed as ident
            Pat::Ident(ident) => quote!(#ident),
            Pat::Path(path) => quote!(#path),
            Pat::TupleStruct(PatTupleStruct { path, .. }) => quote!(#path),
            p => panic!("invalid pattern in handler: {p:?}"),
        };
        let new_pat = if generics.is_some() {
            match &pat {
                Pat::Ident(ident) => quote!(#ident(::core::marker::PhantomData)),
                Pat::Path(path) => quote!(#path(::core::marker::PhantomData)),
                Pat::TupleStruct(p) => {
                    let mut p = p.clone();
                    p.pat.elems.push(parse_quote!(::core::marker::PhantomData));
                    quote!(#p)
                },
                p => panic!("invalid pattern in handler: {p:?}"),
            }
        } else {
            quote!(#pat)
        };
        if let Expr::Break(_) | Expr::Return(_) = body {
            body = parse_quote!({ #body });
        }
        syn::visit_mut::visit_expr_mut(
            &mut FixControlFlow {
                eff_ty: &eff_ty,
                is_shorthand,
            },
            &mut body,
        );
        if is_shorthand {
            matcher = quote! {
                {
                    let #new_pat = effs;
                    #body
                }
            };
        } else {
            matcher = quote! {
                match effs.uninject() {
                    Ok(#new_pat) => {
                        let __effing_inj = #body;
                        #[allow(unreachable_code)]
                        ::effing_mad::frunk::Coproduct::inject(
                            ::effing_mad::injection::Tagged::<_, #eff_ty #generics>::new(
                                __effing_inj
                            )
                        )
                    },
                    Err(effs) => #matcher,
                }
            };
        }
    }
    let effs_ty = if is_shorthand {
        quote!(#group)
    } else {
        quote!(<#group as ::effing_mad::EffectGroup>::Effects)
    };
    quote! {
        #moveness |effs: #effs_ty| #asyncness {
            let __effing_inj = #matcher;
            // if the handler unconditionally breaks then this line is unreachable, but we
            // don't want to see a warning for it.
            #[allow(unreachable_code)]
            ::core::ops::ControlFlow::<_, _>::Continue(__effing_inj)
        }
    }
    .into()
}
