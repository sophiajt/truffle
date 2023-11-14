use color_eyre::eyre;
use lsp_server::{ExtractError, Message, Request, RequestId, Response};
use tracing::info;

use super::Server;

pub struct Dispatcher<'a> {
    pub request: Option<Request>,
    pub server: &'a Server,
}

impl Dispatcher<'_> {
    pub fn dispatch<R, F>(&mut self, f: F) -> eyre::Result<&mut Self>
    where
        R: lsp_types::request::Request,
        F: Fn(R::Params) -> eyre::Result<R::Result>,
        R::Params: std::fmt::Debug,
    {
        if self.request.is_none()
            || self
                .request
                .as_ref()
                .is_some_and(|req| req.method != R::METHOD)
        {
            return Ok(self);
        }

        let request = self.request.take().unwrap();

        match cast::<R>(request) {
            Ok((id, params)) => {
                info!("got request #{id}: {params:?}");
                let result = f(params)?;
                let result = Some(serde_json::to_value(result)?);
                let resp = Response {
                    id,
                    result,
                    error: None,
                };
                self.server
                    .connection
                    .sender
                    .send(Message::Response(resp))
                    .unwrap();
                return Ok(self);
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(req)) => req,
        };

        Ok(self)
    }
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
