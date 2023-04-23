mod delta;
mod errors;
mod lexer;
mod line_editor;
mod parser;
mod typechecker;

use crate::{parser::Parser, typechecker::TypeChecker};
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

                let mut typechecker = TypeChecker::new(parser.delta.ast_nodes.len());
                typechecker.typecheck(&parser.delta);

                for error in &typechecker.errors {
                    println!("error: {:?}", error);
                }

                let result = &parser.delta;
                result.print();

                for ast_node in result.ast_nodes.iter().enumerate() {
                    println!("{:?}", ast_node)
                }

                for node_type in typechecker.node_types.iter().enumerate() {
                    println!("{:?}", node_type)
                }
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}
