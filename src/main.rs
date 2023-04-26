mod codegen;
mod delta;
mod errors;
mod lexer;
mod line_editor;
mod parser;
mod typechecker;

use crate::{codegen::Translater, parser::Parser, typechecker::TypeChecker};
use line_editor::{LineEditor, ReadLineOutput};

fn main() {
    let args = std::env::args();

    if args.len() > 1 {
        for arg in args.skip(1) {
            let contents = std::fs::read_to_string(arg).expect("couldn't find file");

            run_line(&contents);
        }
        return;
    }

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

                run_line(&line);
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}

fn run_line(line: &str) {
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

    let mut idx = 0;
    while idx < result.ast_nodes.len() {
        println!(
            "{}: {:?} ({})",
            idx,
            result.ast_nodes[idx],
            typechecker.stringify_type(typechecker.node_types[idx])
        );
        idx += 1;
    }

    let mut translater = Translater::new();

    let mut output = translater.translate(&parser.delta, &typechecker);

    output.debug_print();

    println!("result: {:?}", output.eval());
}
