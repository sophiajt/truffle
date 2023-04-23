mod line_editor;
use line_editor::{LineEditor, ReadLineOutput};

mod lexer;
use lexer::Lexer;

use crate::parser::Parser;

mod parser;
mod parser_delta;

mod errors;

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
