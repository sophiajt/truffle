use color_eyre::eyre;
use lsp_server::{ExtractError, Message, Notification, Request, Response};
use tracing::info;

use super::Server;

pub struct Dispatcher<'a, T> {
    pub request: Option<T>,
    pub server: &'a Server,
}

impl Dispatcher<'_, Request> {
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

        match request.extract::<R::Params>(R::METHOD) {
            Ok((id, params)) => {
                info!("got request #{id}: {params:?}");
                let result = f(params)?;
                let result = Some(serde_json::to_value(result)?);
                let resp = Response {
                    id,
                    result,
                    error: None,
                };
                info!("sending back response: {resp:?}");
                self.server
                    .connection
                    .sender
                    .send(Message::Response(resp))
                    .unwrap();
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => {
                unreachable!("method should be verified before extracting")
            }
        };

        Ok(self)
    }
}

impl Dispatcher<'_, Notification> {
    pub fn dispatch<R, F>(&mut self, mut f: F) -> eyre::Result<&mut Self>
    where
        R: lsp_types::notification::Notification,
        F: FnMut(R::Params) -> eyre::Result<()>,
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

        let notification = self.request.take().unwrap();

        match notification.extract::<R::Params>(R::METHOD) {
            Ok(params) => {
                info!("got notification {params:?}");
                f(params)?;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => {
                unreachable!("method should be verified before extracting")
            }
        };

        Ok(self)
    }
}
