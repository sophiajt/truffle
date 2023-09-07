use std::error::Error;

#[cfg(feature = "reedline")]
pub struct TrufflePrompt {}

#[allow(unused)]
pub enum ReadLineOutput {
    Continue,
    Break,
    Success(String),
}

#[cfg(feature = "reedline")]
impl reedline::Prompt for TrufflePrompt {
    fn render_prompt_left(&self) -> std::borrow::Cow<str> {
        "".into()
    }

    fn render_prompt_right(&self) -> std::borrow::Cow<str> {
        "".into()
    }

    fn render_prompt_indicator(
        &self,
        _prompt_mode: reedline::PromptEditMode,
    ) -> std::borrow::Cow<str> {
        "> ".into()
    }

    fn render_prompt_multiline_indicator(&self) -> std::borrow::Cow<str> {
        ": ".into()
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reedline::PromptHistorySearch,
    ) -> std::borrow::Cow<str> {
        "?> ".into()
    }
}

#[cfg(feature = "reedline")]
pub struct LineEditor {
    inner: reedline::Reedline,
}

#[cfg(feature = "reedline")]
impl LineEditor {
    pub fn new() -> LineEditor {
        let line_editor = reedline::Reedline::create();
        LineEditor { inner: line_editor }
    }

    pub fn read_line(&mut self) -> Result<ReadLineOutput, Box<dyn Error>> {
        use reedline::Signal;

        let result = self.inner.read_line(&TrufflePrompt {});
        match result {
            Ok(Signal::CtrlC) => Ok(ReadLineOutput::Continue),
            Ok(Signal::CtrlD) => Ok(ReadLineOutput::Break),
            Ok(Signal::Success(line)) => Ok(ReadLineOutput::Success(line)),
            Err(err) => Err(Box::new(err)),
        }
    }
}

#[cfg(not(feature = "reedline"))]
pub struct LineEditor {}

#[cfg(not(feature = "reedline"))]
impl LineEditor {
    pub fn new() -> LineEditor {
        LineEditor {}
    }

    pub fn read_line(&mut self) -> Result<ReadLineOutput, Box<dyn Error>> {
        use std::io::{stdin, stdout, Write};

        let mut buffer = String::new();
        let stdin = stdin();
        let mut stdout = stdout();
        print!("> ");
        let _ = stdout.flush();

        let result = stdin.read_line(&mut buffer);

        match result {
            Ok(_) => {
                buffer = buffer.trim_end().to_string();
                Ok(ReadLineOutput::Success(buffer))
            }
            Err(err) => Err(Box::new(err)),
        }
    }
}
