mod errors;
mod lexer;
mod line_editor;
mod parser;
mod parser_delta;

use crate::parser::Parser;
use line_editor::{LineEditor, ReadLineOutput};

fn main() {
    let mut line_editor = LineEditor::new();

    loop {
        match line_editor.read_line() {
            Ok(ReadLineOutput::Continue) => {
                continue;
            }
            Ok(ReadLineOutput::Break) => {
                break;
            }
            Ok(ReadLineOutput::Success(line)) => {
                if line == "exit" || line == "quit" {
                    break;
                }
                println!("line: {line}");

                let mut parser = Parser::new(line.as_bytes(), 0, 0);

                parser.parse();

                for error in &parser.errors {
                    println!("error: {:?}", error);
                }

                let result = parser.delta;

                result.print();
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}
