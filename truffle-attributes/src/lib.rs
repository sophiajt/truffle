use proc_macro::TokenStream;
use quote::quote;
use syn::ItemFn;

#[proc_macro_attribute]
pub fn register_async_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    foo(item).unwrap().into()
}

fn foo(item: TokenStream) -> Result<proc_macro2::TokenStream, syn::Error> {
    let input = syn::parse::<ItemFn>(item.clone())?;
    let register_fn = generate::register_fn(input.clone())?;

    let output = quote! {
        #input

        #register_fn
    };
    Ok(output)
}

mod generate {
    use proc_macro2::TokenStream;
    use syn::{ItemFn, token::Mut};
    use quote::{quote, format_ident};

    pub fn register_fn(input: ItemFn) -> Result<TokenStream, syn::Error> {
        let wrapped_fn_name = input.sig.ident.to_string();
        let register_fn_name = format_ident!("register_{wrapped_fn_name}");
        let wrapped_fn = wrapped_fn(input.clone())?;
        let registration_closure = registration_closure(input)?;

        Ok(quote! {
            fn #register_fn_name() -> impl Fn(&mut truffle::TypeChecker) {
                use futures::FutureExt;

                #wrapped_fn

                #registration_closure
            }
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

                    pattype.ty = Box::new(syn::parse_str("Box<dyn std::any::Any + Send>").unwrap());
                }
            }
            arg
        });

        let idents = input.sig.inputs.iter().map(|arg| {
            match arg {
                syn::FnArg::Receiver(_) => todo!(),
                syn::FnArg::Typed(pattype) => {
                    match &*pattype.pat {
                        syn::Pat::Const(_) => todo!(),
                        syn::Pat::Ident(patident) => {
                            &patident.ident
                        }
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
                }
            }
        });

        let converted_args = idents
            .clone()
            .map(|ident| quote! { let #ident = #ident.downcast_mut().unwrap(); });

        Ok(quote! {
            fn wrapped_fn(
                #(#wrapped_params)*
            ) -> futures::future::BoxFuture<'static, Result<Box<dyn Any>, String>> {
                async move {
                    #(
                    #converted_args
                    )*
                    Ok(Box::new(#wrapped_fn_name(#(*#idents),*).await) as Box<dyn Any>)
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
            quote! { typechecker.get_type::<#ty>().unwrap() }
        });

        let ret_type = match input.sig.output {
            syn::ReturnType::Default => syn::parse_str("()")?,
            syn::ReturnType::Type(_, ty) => ty,
        };

        Ok(quote! {
            |typechecker: &mut TypeChecker| {
                typechecker.add_async_call(
                    vec![#(#param_types)*],
                    typechecker.get_type::<#ret_type>().unwrap(),
                    Function::ExternalAsyncFn1(wrapped_fn),
                    #wrapped_fn_name,
                );
            }
        })
    }
}
