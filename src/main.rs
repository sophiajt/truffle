mod line_editor;
use line_editor::{LineEditor, ReadLineOutput};

mod lexer;
use lexer::Lexer;

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

                let mut lexer = Lexer::new(line.as_bytes(), 0);

                while let Some(token) = lexer.next() {
                    println!("token: {:?}", token);
                }
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}
