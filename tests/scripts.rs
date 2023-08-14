use libtest_mimic::{Arguments, Failed, Trial};
use truffle::ReturnValue;

use std::{
    env,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

mod test_eval;

fn main() -> anyhow::Result<()> {
    let args = Arguments::from_args();
    let tests = collect_tests()?;
    libtest_mimic::run(&args, tests).exit();
}

/// Creates one test for each `.truffle` file in the current directory or
/// sub-directories of the current directory.
fn collect_tests() -> anyhow::Result<Vec<Trial>> {
    fn visit_dir(path: &Path, tests: &mut Vec<Trial>) -> anyhow::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let file_type = entry.file_type()?;

            // Handle files
            let path = entry.path();
            if file_type.is_file() {
                if path.extension() == Some(OsStr::new("truffle")) {
                    let name = path
                        .strip_prefix(env::current_dir()?)?
                        .display()
                        .to_string();

                    let test = Trial::test(name, move || eval_source_runner(&path));
                    tests.push(test);
                }
            } else if file_type.is_dir() {
                // Handle directories
                visit_dir(&path, tests)?;
            }
        }

        Ok(())
    }

    // We recursively look for `.truffle` files, starting from the current
    // directory.
    let mut tests = Vec::new();
    let current_dir = env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)?
        .join("tests");
    visit_dir(&current_dir, &mut tests)?;

    Ok(tests)
}

/// testrunner adapter for libtest-mimic
pub fn eval_source_runner(fname: &Path) -> Result<(), Failed> {
    let source = fs::read(fname).map_err(|e| format!("Cannot read file: {e}"))?;
    let source = String::from_utf8(source)
        .map_err(|_| "The file's contents are not a valid UTF-8 string!")?;
    match test_eval::eval_source(&source) {
        Ok(ReturnValue::Custom { value: 0, .. }) => Ok(()),
        Ok(non_zero) => Err(format!("Script evaluated to {non_zero:?}, expected 0"))?,
        Err(errors) => {
            let contents = source.as_bytes();
            errors.print_with(fname, contents);
            Err(format!("Script is invalid"))?
        }
    }
}
