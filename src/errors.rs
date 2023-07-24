#[derive(Debug)]
pub struct ScriptError {
    pub message: String,
    pub span_start: usize,
    pub span_end: usize,
}

impl ScriptError {
    // pub fn new(message: impl Into<String>, node_id: NodeId) -> ScriptError {
    //     ScriptError {
    //         message: message.into(),
    //         node_id,
    //     }
    // }
}

pub fn print_error(fname: &str, script_error: &ScriptError, contents: &[u8]) {
    let ScriptError {
        message,
        span_start,
        span_end,
    } = script_error;

    let span_start = *span_start;
    let span_end = *span_end;

    let filename = fname.to_string();
    let file_span_start = 0;
    let file_span_end = contents.len();

    let (line_number, line_start, line_end) =
        line_extents(contents, span_start, file_span_start, file_span_end);

    let line_number_width = format!("{}", line_number).len();

    let max_number_width = if (line_end + 1) < file_span_end {
        let (next_line_number, _, _) =
            line_extents(contents, line_end + 1, file_span_start, file_span_end);
        format!("{}", next_line_number).len()
    } else {
        line_number_width
    };

    for _ in 0..(max_number_width + 2) {
        eprint!("─");
    }
    eprintln!(
        "┬─ \x1b[0;36m{}:{}:{}\x1b[0m",
        filename,
        line_number,
        span_start - line_start + 1
    );

    // Previous line in the source code, if available
    if line_start > (file_span_start + 1) {
        let (prev_line_number, prev_line_start, prev_line_end) =
            line_extents(contents, line_start - 1, file_span_start, file_span_end);
        let prev_line_number_str = format!("{}", prev_line_number);

        for _ in 0..(max_number_width - prev_line_number_str.len()) {
            eprint!(" ")
        }

        eprintln!(
            " {} │ {}",
            prev_line_number_str,
            String::from_utf8_lossy(&contents[prev_line_start..prev_line_end])
        );
    }

    // Line being highlighted
    for _ in 0..(max_number_width - line_number_width) {
        eprint!(" ")
    }

    eprintln!(
        " {} │ {}",
        line_number,
        String::from_utf8_lossy(&contents[line_start..line_end])
    );

    for _ in 0..(max_number_width + 2) {
        eprint!(" ");
    }
    eprint!("│");
    for _ in 0..(span_start - line_start + 1) {
        eprint!(" ");
    }

    eprint!("\x1b[0;31m");
    for _ in span_start..span_end {
        eprint!("╍");
    }
    eprintln!(" error: {}", message);
    eprint!("\x1b[0m");

    // Next line after error, for context
    if (line_end + 1) < file_span_end {
        let (next_line_number, next_line_start, next_line_end) =
            line_extents(contents, line_end + 1, file_span_start, file_span_end);

        eprintln!(
            " {} │ {}",
            next_line_number,
            String::from_utf8_lossy(&contents[next_line_start..next_line_end])
        );
    }

    for _ in 0..(max_number_width + 2) {
        eprint!("─");
    }
    eprintln!("┴─");
}

// line number, line start, line_end
pub fn line_extents(
    contents: &[u8],
    span_position: usize,
    file_span_start: usize,
    file_span_end: usize,
) -> (usize, usize, usize) {
    let line_number = contents[0..span_position].split(|x| *x == b'\n').count();

    let mut line_start = span_position;
    if line_start > file_span_start && contents[line_start] == b'\n' {
        line_start -= 1;
    }

    while line_start > file_span_start && contents[line_start] != b'\n' {
        line_start -= 1;
    }
    if contents[line_start] == b'\n' {
        line_start += 1;
    }

    let mut line_end = span_position;
    while line_end < file_span_end && contents[line_end] != b'\n' {
        line_end += 1;
    }

    (line_number, line_start, line_end)
}
