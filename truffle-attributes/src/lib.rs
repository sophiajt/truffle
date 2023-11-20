use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse::Parse, parse_macro_input, Expr, ExprPath, ItemFn, Token};

struct RegisterFnInput {
    engine: Expr,
    name: Expr,
    fun: ExprPath,
}

impl Parse for RegisterFnInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let engine = input.parse()?;
        input.parse::<Token![,]>()?;
        let name = input.parse()?;
        input.parse::<Token![,]>()?;
        let fun = input.parse()?;
        Ok(RegisterFnInput { engine, name, fun })
    }
}

#[proc_macro]
pub fn register_fn(input: TokenStream) -> TokenStream {
    let RegisterFnInput { engine, name, fun } = parse_macro_input!(input);
    let fun_is_async = {
        let mut path = fun.path.clone();
        let ident_segment = path
            .segments
            .last_mut()
            .expect("path should always have at least one segment");
        ident_segment.ident = format_ident!("{}_is_async", ident_segment.ident);
        path
    };
    let register_fun = {
        let mut path = fun.path.clone();
        let ident_segment = path
            .segments
            .last_mut()
            .expect("path should always have at least one segment");
        ident_segment.ident = format_ident!("register_{}", ident_segment.ident);
        path
    };
    let register_lsp_info = {
        let mut path = fun.path.clone();
        let ident_segment = path
            .segments
            .last_mut()
            .expect("path should always have at least one segment");
        ident_segment.ident = format_ident!("register_lsp_info_{}", ident_segment.ident);
        path
    };

    quote! {
        engine.with(#register_lsp_info());
        if #fun_is_async() {
            #engine.with(#register_fun())
        } else {
            #engine.register_fn(#name, #fun)
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn export(_attr: TokenStream, item: TokenStream) -> TokenStream {
    foo(item).unwrap().into()
}

fn foo(item: TokenStream) -> Result<proc_macro2::TokenStream, syn::Error> {
    let input = syn::parse::<ItemFn>(item.clone())?;
    let output = if input.sig.asyncness.is_some() {
        let register_fn = generate::register_fn(input.clone())?;
        let fn_is_async = generate::fn_is_async(input.clone())?;

        quote! {
            #input

            #register_fn
            #fn_is_async
        }
    } else {
        let register_fn = generate::register_fn_stub(input.clone()).expect("stub should generate");
        let fn_is_async = generate::fn_is_async(input.clone())?;
        quote! {
            #input

            #register_fn
            #fn_is_async
        }
    };
    Ok(output)
}

mod generate {
    use proc_macro2::TokenStream;
    use quote::{format_ident, quote};
    use syn::{parse_quote, token::Mut, ItemFn};

    pub fn register_fn(input: ItemFn) -> Result<TokenStream, syn::Error> {
        let wrapped_fn_name = input.sig.ident.to_string();
        let register_fn_name = format_ident!("register_{wrapped_fn_name}");
        let wrapped_fn = wrapped_fn(input.clone())?;
        let registration_closure = registration_closure(input)?;

        Ok(quote! {
            fn #register_fn_name() -> impl Fn(&mut truffle::Engine) {
                use futures::FutureExt;

                #wrapped_fn

                #registration_closure
            }
        })
    }

    pub fn register_fn_stub(input: ItemFn) -> Result<TokenStream, syn::Error> {
        let wrapped_fn_name = input.sig.ident.to_string();
        let mut register_fn_stub = input;
        register_fn_stub.sig.ident = format_ident!("register_{wrapped_fn_name}");
        register_fn_stub.sig.output = syn::parse_str("-> impl Fn(&mut truffle::Engine)")
            .expect("this should parse as a return type");
        register_fn_stub.sig.inputs = Default::default();
        register_fn_stub.block = syn::parse_str(
            "{|engine| unreachable!(\"register fn should only be called for async fns\")}",
        )
        .expect("this should parse as a block body for a function");

        Ok(quote! {
            #register_fn_stub
        })
    }

    pub fn fn_is_async(input: ItemFn) -> Result<TokenStream, syn::Error> {
        let wrapped_fn_name = input.sig.ident.to_string();
        let is_async = input.sig.asyncness.is_some();
        let mut fn_is_async = input;
        fn_is_async.sig.asyncness = None;
        fn_is_async.sig.ident = format_ident!("{wrapped_fn_name}_is_async");
        fn_is_async.sig.output =
            syn::parse_str("-> bool").expect("this should parse as a return type");
        fn_is_async.sig.inputs = Default::default();
        fn_is_async.block = parse_quote! {
            {
                #is_async
            }
        };

        Ok(quote! {
            #fn_is_async
        })
    }

    fn wrapped_fn(input: ItemFn) -> Result<TokenStream, syn::Error> {
        let wrapped_fn_name = input.sig.ident;
        let wrapped_params = input.sig.inputs.iter().map(|arg| {
            let mut arg = arg.clone();
            match &mut arg {
                syn::FnArg::Receiver(_) => todo!(),
                syn::FnArg::Typed(pattype) => {
                    match &mut *pattype.pat {
                        syn::Pat::Const(_) => todo!(),
                        syn::Pat::Ident(patident) => patident.mutability = Some(Mut::default()),
                        syn::Pat::Lit(_) => todo!(),
                        syn::Pat::Macro(_) => todo!(),
                        syn::Pat::Or(_) => todo!(),
                        syn::Pat::Paren(_) => todo!(),
                        syn::Pat::Path(_) => todo!(),
                        syn::Pat::Range(_) => todo!(),
                        syn::Pat::Reference(_) => todo!(),
                        syn::Pat::Rest(_) => todo!(),
                        syn::Pat::Slice(_) => todo!(),
                        syn::Pat::Struct(_) => todo!(),
                        syn::Pat::Tuple(_) => todo!(),
                        syn::Pat::TupleStruct(_) => todo!(),
                        syn::Pat::Type(_) => todo!(),
                        syn::Pat::Verbatim(_) => todo!(),
                        syn::Pat::Wild(_) => todo!(),
                        _ => todo!(),
                    }

                    pattype.ty = Box::new(
                        syn::parse_str("Box<dyn std::any::Any + Send>")
                            .expect("input should be a valid rust type"),
                    );
                }
            }
            arg
        });

        let idents = input.sig.inputs.iter().map(|arg| match arg {
            syn::FnArg::Receiver(_) => todo!(),
            syn::FnArg::Typed(pattype) => match &*pattype.pat {
                syn::Pat::Const(_) => todo!(),
                syn::Pat::Ident(patident) => &patident.ident,
                syn::Pat::Lit(_) => todo!(),
                syn::Pat::Macro(_) => todo!(),
                syn::Pat::Or(_) => todo!(),
                syn::Pat::Paren(_) => todo!(),
                syn::Pat::Path(_) => todo!(),
                syn::Pat::Range(_) => todo!(),
                syn::Pat::Reference(_) => todo!(),
                syn::Pat::Rest(_) => todo!(),
                syn::Pat::Slice(_) => todo!(),
                syn::Pat::Struct(_) => todo!(),
                syn::Pat::Tuple(_) => todo!(),
                syn::Pat::TupleStruct(_) => todo!(),
                syn::Pat::Type(_) => todo!(),
                syn::Pat::Verbatim(_) => todo!(),
                syn::Pat::Wild(_) => todo!(),
                _ => todo!(),
            },
        });

        let converted_args = idents
            .clone()
            .map(|ident| quote! { let #ident = #ident.downcast_mut().expect("downcast type should match the actual type"); });

        Ok(quote! {
            fn wrapped_fn(
                #(#wrapped_params)*
            ) -> futures::future::BoxFuture<'static, Result<Box<dyn std::any::Any>, String>> {
                async move {
                    #(
                    #converted_args
                    )*
                    Ok(Box::new(#wrapped_fn_name(#(*#idents),*).await) as Box<dyn std::any::Any>)
                }
                .boxed()
            }
        })
    }

    fn registration_closure(input: ItemFn) -> Result<TokenStream, syn::Error> {
        let wrapped_fn_name = input.sig.ident.to_string();

        let param_types = input.sig.inputs.iter().map(|arg| {
            match arg {
                syn::FnArg::Receiver(_) => todo!(),
                syn::FnArg::Typed(pattype) => {
                    &pattype.ty
                }
            }
        }).map(|ty| {
            quote! { engine.get_type::<#ty>().expect("engine should already know about this type") }
        });

        let ret_type = match input.sig.output {
            syn::ReturnType::Default => syn::parse_str("()")?,
            syn::ReturnType::Type(_, ty) => ty,
        };

        Ok(quote! {
            |engine: &mut Engine| {
                use truffle::Function;

                engine.add_async_call(
                    vec![#(#param_types)*],
                    engine.get_type::<#ret_type>().expect("engine should already know about this type"),
                    Function::ExternalAsyncFn1(wrapped_fn),
                    #wrapped_fn_name,
                );
            }
        })
    }
}
